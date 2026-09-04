import gleam/list
import gleam/option.{type Option, None, Some}
import lib_dataflow as dataflow

@external(javascript, "./event_semaphore_ffi.mjs", "schedule")
fn schedule(callback: fn() -> Nil) -> Nil

type WaiterState {
  Waiting
  Scheduled
}

type Waiter {
  Waiter(id: Int, state: WaiterState, callback: fn() -> Nil)
}

type SemaphoreState {
  SemaphoreState(resolved: Bool, next_id: Int, waiters: List(Waiter))
}

pub opaque type EventSemaphore {
  EventSemaphore(
    context: dataflow.Context,
    state: dataflow.Mutation(SemaphoreState),
  )
}

pub fn event_semaphore() -> EventSemaphore {
  EventSemaphore(
    context: dataflow.dataflow(),
    state: dataflow.state(SemaphoreState(False, 0, [])),
  )
}

// `ready` is the explicit Gleam form of the source's Promise-returning
// getter. The callback receives the same semaphore so state transitions made
// by a callback can be threaded through its operation sequence naturally.
pub fn ready(
  semaphore: EventSemaphore,
  done: fn(EventSemaphore) -> Nil,
) -> EventSemaphore {
  let EventSemaphore(context, state) = semaphore
  let #(id, should_schedule) =
    update(context, state, fn(current) {
      let SemaphoreState(resolved, next_id, waiters) = current
      let waiter_state = case resolved {
        True -> Scheduled
        False -> Waiting
      }
      let waiter = Waiter(next_id, waiter_state, fn() { done(semaphore) })
      #(
        #(next_id, resolved),
        SemaphoreState(resolved, next_id + 1, list.append(waiters, [waiter])),
      )
    })
  case should_schedule {
    True -> schedule_waiter(semaphore, state, id)
    False -> Nil
  }
  semaphore
}

pub fn clear(semaphore: EventSemaphore) -> EventSemaphore {
  let EventSemaphore(context, state) = semaphore
  let _ =
    update(context, state, fn(current) {
      let SemaphoreState(resolved, next_id, waiters) = current
      #(Nil, case resolved {
        True -> SemaphoreState(False, next_id, waiters)
        False -> current
      })
    })
  semaphore
}

pub fn set(semaphore: EventSemaphore) -> EventSemaphore {
  let EventSemaphore(context, state) = semaphore
  let scheduled =
    update(context, state, fn(current) {
      let SemaphoreState(resolved, next_id, waiters) = current
      case resolved {
        True -> #([], current)
        False -> {
          let #(waiters, ids) = schedule_waiters(waiters, [], [])
          #(ids, SemaphoreState(True, next_id, waiters))
        }
      }
    })
  schedule_ids(semaphore, state, scheduled)
  semaphore
}

fn update(
  context: dataflow.Context,
  state: dataflow.Mutation(SemaphoreState),
  change: fn(SemaphoreState) -> #(output, SemaphoreState),
) -> output {
  let assert Ok(output) =
    dataflow.txn(context, fn() {
      let current =
        dataflow.value(dataflow.to_computation(dataflow.mutation(state)))
      let #(output, next) = change(current)
      let assert Ok(Nil) = dataflow.set(context, state, next)
      Ok(output)
    })
  output
}

fn schedule_waiter(
  semaphore: EventSemaphore,
  state: dataflow.Mutation(SemaphoreState),
  id: Int,
) -> Nil {
  schedule(fn() { fire(semaphore, state, id) })
}

fn schedule_ids(
  semaphore: EventSemaphore,
  state: dataflow.Mutation(SemaphoreState),
  ids: List(Int),
) -> Nil {
  case ids {
    [] -> Nil
    [first, ..rest] -> {
      schedule_waiter(semaphore, state, first)
      schedule_ids(semaphore, state, rest)
    }
  }
}

fn fire(
  semaphore: EventSemaphore,
  state: dataflow.Mutation(SemaphoreState),
  id: Int,
) -> Nil {
  let EventSemaphore(context, _) = semaphore
  let callback =
    update(context, state, fn(current) {
      let SemaphoreState(resolved, next_id, waiters) = current
      case take_waiter(waiters, id, []) {
        None -> #(None, current)
        Some(#(waiter, rest)) ->
          case waiter {
            Waiter(_, Waiting, _) -> #(None, current)
            Waiter(_, Scheduled, callback) -> {
              case resolved {
                True -> #(
                  Some(callback),
                  SemaphoreState(resolved, next_id, rest),
                )
                False -> #(
                  None,
                  SemaphoreState(resolved, next_id, mark_waiting(waiters, id)),
                )
              }
            }
          }
      }
    })
  case callback {
    None -> Nil
    Some(callback) -> callback()
  }
}

fn schedule_waiters(
  waiters: List(Waiter),
  output: List(Waiter),
  ids: List(Int),
) -> #(List(Waiter), List(Int)) {
  case waiters {
    [] -> #(list.reverse(output), list.reverse(ids))
    [first, ..rest] ->
      case first {
        Waiter(id, Waiting, callback) ->
          schedule_waiters(rest, [Waiter(id, Scheduled, callback), ..output], [
            id,
            ..ids
          ])
        Waiter(_, Scheduled, _) ->
          schedule_waiters(rest, [first, ..output], ids)
      }
  }
}

fn take_waiter(
  waiters: List(Waiter),
  id: Int,
  before: List(Waiter),
) -> Option(#(Waiter, List(Waiter))) {
  case waiters {
    [] -> None
    [first, ..rest] ->
      case first {
        Waiter(first_id, _, _) if first_id == id ->
          Some(#(first, list.reverse(before) |> append(rest)))
        _ -> take_waiter(rest, id, [first, ..before])
      }
  }
}

fn mark_waiting(waiters: List(Waiter), id: Int) -> List(Waiter) {
  case waiters {
    [] -> []
    [first, ..rest] ->
      case first {
        Waiter(first_id, Scheduled, callback) if first_id == id -> [
          Waiter(first_id, Waiting, callback),
          ..rest
        ]
        _ -> [first, ..mark_waiting(rest, id)]
      }
  }
}

fn append(first: List(a), second: List(a)) -> List(a) {
  case first {
    [] -> second
    [head, ..tail] -> [head, ..append(tail, second)]
  }
}
