import gleam/option.{type Option, None, Some}
import lib_dataflow as dataflow

// The source scheduler serializes work through a Promise tail. The queue and
// its completion state are ordinary library logic, so they live in Gleam;
// only the microtask turn used for completed waiters crosses the JavaScript
// runtime boundary.
pub opaque type Scheduler {
  Scheduler(context: dataflow.Context, state: dataflow.Mutation(State))
}

type State {
  State(
    active: Bool,
    failed: Option(String),
    scheduled: Bool,
    tasks: List(Task),
    waiters: List(Waiter),
  )
}

type Task {
  Sync(fn() -> Nil)
  Async(fn(fn() -> Nil) -> Nil)
  AsyncResult(fn(fn(Result(Nil, String)) -> Nil) -> Nil)
}

type Waiter {
  Wait(fn() -> Nil)
  WaitResult(fn(Result(Nil, String)) -> Nil)
}

@external(javascript, "./scheduler_ffi.mjs", "queue_microtask")
fn queue_microtask(callback: fn() -> Nil) -> Nil

pub fn new() -> Scheduler {
  let context = dataflow.dataflow()
  Scheduler(
    context,
    dataflow.state(
      State(
        active: False,
        failed: None,
        scheduled: False,
        tasks: [],
        waiters: [],
      ),
    ),
  )
}

pub fn run(scheduler: Scheduler, callback: fn() -> Nil) -> Nil {
  enqueue(scheduler, Sync(callback))
}

pub fn run_async(
  scheduler: Scheduler,
  callback: fn(fn() -> Nil) -> Nil,
) -> Nil {
  enqueue(scheduler, Async(callback))
}

pub fn run_async_result(
  scheduler: Scheduler,
  callback: fn(fn(Result(Nil, String)) -> Nil) -> Nil,
) -> Nil {
  enqueue(scheduler, AsyncResult(callback))
}

pub fn wait(scheduler: Scheduler, callback: fn() -> Nil) -> Nil {
  let Scheduler(context, state) = scheduler
  let current = value(state)
  case current.failed, current.active, current.tasks {
    Some(_), _, _ -> Nil
    None, False, [] -> queue_microtask(callback)
    None, _, _ -> {
      set_state(
        context,
        state,
        State(..current, waiters: append(current.waiters, [Wait(callback)])),
      )
      Nil
    }
  }
}

pub fn wait_result(
  scheduler: Scheduler,
  callback: fn(Result(Nil, String)) -> Nil,
) -> Nil {
  let Scheduler(context, state) = scheduler
  let current = value(state)
  case current.failed, current.active, current.tasks {
    Some(reason), _, _ -> queue_microtask(fn() { callback(Error(reason)) })
    None, False, [] -> queue_microtask(fn() { callback(Ok(Nil)) })
    None, _, _ -> {
      set_state(
        context,
        state,
        State(
          ..current,
          waiters: append(current.waiters, [WaitResult(callback)]),
        ),
      )
      Nil
    }
  }
}

fn enqueue(scheduler: Scheduler, task: Task) -> Nil {
  let Scheduler(context, state) = scheduler
  let current = value(state)
  case current.failed {
    Some(_) -> Nil
    None -> {
      set_state(
        context,
        state,
        State(..current, tasks: append(current.tasks, [task])),
      )
      schedule_process(scheduler)
    }
  }
}

fn process(scheduler: Scheduler) -> Nil {
  let Scheduler(context, state) = scheduler
  let current = value(state)
  set_state(context, state, State(..current, scheduled: False))
  case current.failed, current.active, current.tasks {
    Some(_), _, _ -> Nil
    None, True, _ -> Nil
    None, False, [] -> Nil
    None, False, [task, ..rest] -> {
      set_state(context, state, State(..current, active: True, tasks: rest))
      launch(scheduler, task)
    }
  }
}

fn launch(scheduler: Scheduler, task: Task) -> Nil {
  case task {
    Sync(callback) -> {
      callback()
      complete(scheduler, Ok(Nil))
    }
    Async(callback) -> {
      let Scheduler(context, _) = scheduler
      let finished = dataflow.state(False)
      callback(fn() { finish_once(scheduler, context, finished, Ok(Nil)) })
    }
    AsyncResult(callback) -> {
      let Scheduler(context, _) = scheduler
      let finished = dataflow.state(False)
      callback(fn(result) { finish_once(scheduler, context, finished, result) })
    }
  }
}

fn finish_once(
  scheduler: Scheduler,
  context: dataflow.Context,
  finished: dataflow.Mutation(Bool),
  result: Result(Nil, String),
) -> Nil {
  let already_finished =
    dataflow.value(dataflow.to_computation(dataflow.mutation(finished)))
  case already_finished {
    True -> Nil
    False -> {
      set_state(context, finished, True)
      complete(scheduler, result)
    }
  }
}

fn complete(scheduler: Scheduler, result: Result(Nil, String)) -> Nil {
  let Scheduler(context, state) = scheduler
  let current = value(state)
  case result {
    Ok(_) -> {
      case current.tasks {
        [] -> {
          set_state(
            context,
            state,
            State(..current, active: False, waiters: []),
          )
          notify(current.waiters, Ok(Nil))
        }
        _ -> {
          set_state(context, state, State(..current, active: False))
          schedule_process(scheduler)
        }
      }
    }
    Error(reason) -> {
      set_state(
        context,
        state,
        State(
          active: False,
          failed: Some(reason),
          scheduled: False,
          tasks: [],
          waiters: [],
        ),
      )
      notify(current.waiters, Error(reason))
    }
  }
}

fn schedule_process(scheduler: Scheduler) -> Nil {
  let Scheduler(context, state) = scheduler
  let current = value(state)
  case current.failed, current.active, current.scheduled, current.tasks {
    None, False, False, [_first, ..] -> {
      set_state(context, state, State(..current, scheduled: True))
      queue_microtask(fn() { process(scheduler) })
    }
    _, _, _, _ -> Nil
  }
}

fn notify(waiters: List(Waiter), result: Result(Nil, String)) -> Nil {
  case waiters {
    [] -> Nil
    _ -> queue_microtask(fn() { notify_now(waiters, result) })
  }
}

fn notify_now(waiters: List(Waiter), result: Result(Nil, String)) -> Nil {
  case waiters {
    [] -> Nil
    [first, ..rest] -> {
      case first, result {
        Wait(callback), Ok(_) -> callback()
        Wait(_), Error(_) -> Nil
        WaitResult(callback), result -> callback(result)
      }
      notify_now(rest, result)
    }
  }
}

fn value(state: dataflow.Mutation(State)) -> State {
  dataflow.value(dataflow.to_computation(dataflow.mutation(state)))
}

fn set_state(
  context: dataflow.Context,
  state: dataflow.Mutation(a),
  value: a,
) -> Nil {
  let assert Ok(Nil) =
    dataflow.txn(context, fn() { dataflow.set(context, state, value) })
  Nil
}

fn append(first: List(a), second: List(a)) -> List(a) {
  case first {
    [] -> second
    [head, ..tail] -> [head, ..append(tail, second)]
  }
}
