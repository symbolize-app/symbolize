import gleam/list
import gleam/option.{type Option, None, Some}
import lib_dataflow as dataflow
import lib_error

const high_water_mark = 16

const active_limit = 8

pub type WritableStream(value)

pub type AsyncHandler(value, reason) =
  fn(value) -> lib_error.Async(Nil, reason)

type Waiter(reason) {
  Waiter(resolve: fn() -> Nil, reject: fn(reason) -> Nil)
}

type State(reason) {
  State(active: List(Int), waiters: List(Waiter(reason)), next_id: Int)
}

pub opaque type Sink(value, reason) {
  Sink(
    writable: WritableStream(value),
    context: dataflow.Context,
    state: dataflow.Mutation(State(reason)),
  )
}

// The Web Streams object and its write Promise are foreign values. The
// active-operation set, release queue, and error propagation remain in
// Gleam, matching the source `UnderlyingSinkImpl.write` state machine.
@external(javascript, "./stream_ffi.mjs", "new_sink")
fn new_sink(
  high_water_mark: Int,
  on_write: fn(value, fn() -> Nil, fn(reason) -> Nil) -> Nil,
) -> WritableStream(value)

@external(javascript, "./stream_ffi.mjs", "queue_microtask")
fn queue_microtask(callback: fn() -> Nil) -> Nil

pub fn sink(on_data: fn(value) -> Nil) -> Sink(value, String) {
  let context = dataflow.dataflow()
  let state = dataflow.state(State(active: [], waiters: [], next_id: 0))
  Sink(
    writable: new_sink(high_water_mark, fn(value, resolve, reject) {
      on_data(value)
      let id = begin(context, state, resolve, reject)
      queue_microtask(fn() { complete(context, state, id, Ok(Nil)) })
    }),
    context: context,
    state: state,
  )
}

pub fn async_sink(on_data: AsyncHandler(value, reason)) -> Sink(value, reason) {
  let context = dataflow.dataflow()
  let state = dataflow.state(State(active: [], waiters: [], next_id: 0))
  Sink(
    writable: new_sink(high_water_mark, fn(value, resolve, reject) {
      let operation = on_data(value)
      let id = begin(context, state, resolve, reject)
      operation(fn(result) {
        queue_microtask(fn() { complete(context, state, id, result) })
      })
    }),
    context: context,
    state: state,
  )
}

pub fn writable(sink: Sink(value, reason)) -> WritableStream(value) {
  sink.writable
}

fn begin(
  context: dataflow.Context,
  state: dataflow.Mutation(State(reason)),
  resolve: fn() -> Nil,
  reject: fn(reason) -> Nil,
) -> Int {
  let current =
    dataflow.value(dataflow.to_computation(dataflow.mutation(state)))
  let State(active, waiters, next_id) = current
  let id = next_id
  let active = [id, ..active]
  let waiters = case list.length(active) >= active_limit {
    True -> append(waiters, [Waiter(resolve: resolve, reject: reject)])
    False -> waiters
  }
  let next = State(active: active, waiters: waiters, next_id: id + 1)
  let assert Ok(Nil) =
    dataflow.txn(context, fn() { dataflow.set(context, state, next) })
  case list.length(active) < active_limit {
    True -> resolve()
    False -> Nil
  }
  id
}

fn complete(
  context: dataflow.Context,
  state: dataflow.Mutation(State(reason)),
  id: Int,
  result: Result(Nil, reason),
) -> Nil {
  let current =
    dataflow.value(dataflow.to_computation(dataflow.mutation(state)))
  let State(active, waiters, next_id) = current
  case remove(active, id) {
    None -> Nil
    Some(active) -> {
      let below_limit = list.length(active) < active_limit
      let #(next_waiters, notify) = case result {
        Error(reason) -> #([], fn() { reject_all(waiters, reason) })
        Ok(_) if below_limit -> #([], fn() { resolve_all(waiters) })
        Ok(_) -> #(waiters, fn() { Nil })
      }
      let next = State(active: active, waiters: next_waiters, next_id: next_id)
      let assert Ok(Nil) =
        dataflow.txn(context, fn() { dataflow.set(context, state, next) })
      notify()
    }
  }
}

fn resolve_all(waiters: List(Waiter(reason))) -> Nil {
  case waiters {
    [] -> Nil
    [Waiter(resolve, _), ..rest] -> {
      resolve()
      resolve_all(rest)
    }
  }
}

fn reject_all(waiters: List(Waiter(reason)), reason: reason) -> Nil {
  case waiters {
    [] -> Nil
    [Waiter(_, reject), ..rest] -> {
      reject(reason)
      reject_all(rest, reason)
    }
  }
}

fn remove(items: List(Int), target: Int) -> Option(List(Int)) {
  case items {
    [] -> None
    [first, ..rest] ->
      case first == target {
        True -> Some(rest)
        False ->
          case remove(rest, target) {
            None -> None
            Some(rest) -> Some([first, ..rest])
          }
      }
  }
}

fn append(first: List(a), second: List(a)) -> List(a) {
  case first {
    [] -> second
    [head, ..tail] -> [head, ..append(tail, second)]
  }
}
