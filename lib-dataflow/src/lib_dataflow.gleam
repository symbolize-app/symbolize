import gleam/list
import gleam/option.{type Option, None, Some}

pub type Async(value, reason) =
  fn(fn(Result(value, reason)) -> Nil) -> Nil

pub type DataflowError {
  TransactionRequired
}

pub type Context {
  Context(scheduler: Scheduler)
}

type Ref(value)

type Weak(value)

type ScheduledId {
  ScheduledId(cell: Ref(Nil), subscriber: Int)
}

type Scheduled {
  Scheduled(id: ScheduledId, callback: fn() -> Nil)
}

type Subscriber {
  Subscriber(id: Int, callback: Weak(fn() -> Nil))
}

type KeepAlive {
  KeepAliveNone
  KeepAliveSync(fn() -> Nil)
}

pub opaque type Scheduler {
  Scheduler(
    transactions: Ref(List(List(fn() -> Nil))),
    queue: Ref(List(Scheduled)),
    queued: Ref(List(ScheduledId)),
  )
}

type Cell(value) {
  Cell(
    value: Ref(value),
    revision: Ref(Int),
    subscribers: Ref(List(Subscriber)),
    identity: Ref(Nil),
    subscriber_ids: Ref(Int),
  )
}

type CellSubscription(value) {
  CellSubscription(cell: Cell(value), id: Int)
}

type Memo(value) {
  Memo(revisions: Ref(Option(List(Int))), value: Ref(Option(value)))
}

type MapMemo(input, output) {
  MapMemo(input: Ref(Option(input)), output: Ref(Option(output)))
}

type Map2Memo(first, second, output) {
  Map2Memo(
    first: Ref(Option(first)),
    second: Ref(Option(second)),
    output: Ref(Option(output)),
  )
}

type Map3Memo(first, second, third, output) {
  Map3Memo(
    first: Ref(Option(first)),
    second: Ref(Option(second)),
    third: Ref(Option(third)),
    output: Ref(Option(output)),
  )
}

pub opaque type Subscription {
  Subscription(cancel: fn() -> Nil, keep_alive: KeepAlive)
}

pub opaque type AsyncSubscription(reason) {
  AsyncSubscription(cancel: fn() -> Nil, keep_alive: AsyncKeepAlive(reason))
}

type AsyncKeepAlive(reason) {
  AsyncKeepAliveNone
  AsyncKeepAlive(fn(Int, fn(Result(Nil, reason)) -> Nil) -> Nil)
}

pub opaque type Computation(value) {
  Computation(
    read: fn() -> value,
    revision: fn() -> Int,
    subscribe: fn(fn() -> Nil) -> Subscription,
  )
}

pub opaque type Mutation(value) {
  Mutation(
    computation: Computation(value),
    set: fn(Context, value) -> Result(Nil, DataflowError),
  )
}

pub type NodeOpt(value) {
  Literal(value)
  Reactive(Computation(value))
}

pub opaque type AsyncComputation(value, reason) {
  AsyncComputation(
    update: fn(Int, fn(Result(value, reason)) -> Nil) -> Nil,
    subscribe: fn(fn(Int, fn(Result(Nil, reason)) -> Nil) -> Nil) ->
      AsyncSubscription(reason),
  )
}

type AsyncIteration(input, output) {
  AsyncIteration(epoch_id: Int, dependency: input, output: output)
}

type AsyncMapState(input, output, reason) {
  AsyncMapState(
    iteration: Ref(Option(AsyncIteration(input, output))),
    error: Ref(Option(#(Int, reason))),
    active_epoch: Ref(Option(Int)),
    pending: Ref(List(#(Int, fn(Result(output, reason)) -> Nil))),
    same_dependency: fn(input, input) -> Bool,
    subscribers: Ref(List(AsyncSubscriber(reason))),
    subscriber_ids: Ref(Int),
    dependency_subscription: Ref(Option(AsyncSubscription(reason))),
  )
}

type AsyncSubscriber(reason) {
  AsyncSubscriber(
    id: Int,
    callback: Weak(fn(Int, fn(Result(Nil, reason)) -> Nil) -> Nil),
  )
}

type AsyncState(value, reason) {
  AsyncState(
    value: Ref(value),
    epoch: Ref(Int),
    updated: Ref(Bool),
    subscribers: Ref(List(AsyncSubscriber(reason))),
    subscriber_ids: Ref(Int),
  )
}

type AsyncOperation(reason) {
  AsyncOperation(
    commit: fn(Int) -> Nil,
    update: fn(Int, fn(Result(Nil, reason)) -> Nil) -> Nil,
  )
}

type AsyncJoin2State(first, second, reason) {
  AsyncJoin2State(
    first: Option(first),
    second: Option(second),
    first_finished: Bool,
    second_finished: Bool,
    error: Option(reason),
    settled: Bool,
    done: fn(Result(#(first, second), reason)) -> Nil,
  )
}

type AsyncJoin3State(first, second, third, reason) {
  AsyncJoin3State(
    first: Option(first),
    second: Option(second),
    third: Option(third),
    first_finished: Bool,
    second_finished: Bool,
    third_finished: Bool,
    error: Option(reason),
    settled: Bool,
    done: fn(Result(#(first, second, third), reason)) -> Nil,
  )
}

pub opaque type AsyncContext(reason) {
  AsyncContext(
    next_epoch: Ref(Int),
    transactions: Ref(List(List(AsyncOperation(reason)))),
    in_progress_epoch: Ref(Option(Int)),
    in_progress_count: Ref(Int),
  )
}

pub opaque type AsyncMutation(value, reason) {
  AsyncMutation(
    computation: AsyncComputation(value, reason),
    set: fn(AsyncContext(reason), value) -> Result(Nil, DataflowError),
  )
}

// The TypeScript source exposes one variadic writable `derived` node. Gleam
// has no variadic function types, so these fixed-arity forms preserve the
// source's dependency/value/setter contract without introducing a dynamic
// argument list into the public API.
pub opaque type AsyncDerived1Mutation(input, output, reason) {
  AsyncDerived1Mutation(
    computation: AsyncComputation(output, reason),
    dependency: AsyncComputation(input, reason),
    set: fn(AsyncContext(reason), output, input) -> Async(Nil, reason),
  )
}

pub opaque type AsyncDerived2Mutation(first, second, output, reason) {
  AsyncDerived2Mutation(
    computation: AsyncComputation(output, reason),
    first: AsyncComputation(first, reason),
    second: AsyncComputation(second, reason),
    set: fn(AsyncContext(reason), output, first, second) -> Async(Nil, reason),
  )
}

fn async_all2(
  first: fn(Int, fn(Result(first, reason)) -> Nil) -> Nil,
  second: fn(Int, fn(Result(second, reason)) -> Nil) -> Nil,
  epoch_id: Int,
  done: fn(Result(#(first, second), reason)) -> Nil,
) -> Nil {
  let state =
    new_ref(AsyncJoin2State(
      first: None,
      second: None,
      first_finished: False,
      second_finished: False,
      error: None,
      settled: False,
      done: done,
    ))
  first(epoch_id, fn(result) { async_join2_first(state, result) })
  second(epoch_id, fn(result) { async_join2_second(state, result) })
}

fn async_all3(
  first: fn(Int, fn(Result(first, reason)) -> Nil) -> Nil,
  second: fn(Int, fn(Result(second, reason)) -> Nil) -> Nil,
  third: fn(Int, fn(Result(third, reason)) -> Nil) -> Nil,
  epoch_id: Int,
  done: fn(Result(#(first, second, third), reason)) -> Nil,
) -> Nil {
  let state =
    new_ref(AsyncJoin3State(
      first: None,
      second: None,
      third: None,
      first_finished: False,
      second_finished: False,
      third_finished: False,
      error: None,
      settled: False,
      done: done,
    ))
  first(epoch_id, fn(result) { async_join3_first(state, result) })
  second(epoch_id, fn(result) { async_join3_second(state, result) })
  third(epoch_id, fn(result) { async_join3_third(state, result) })
}

fn async_join2_first(
  state: Ref(AsyncJoin2State(first, second, reason)),
  result: Result(first, reason),
) -> Nil {
  let current = read_ref(state)
  case current.first_finished {
    True -> Nil
    False -> {
      let next = case result {
        Ok(value) ->
          AsyncJoin2State(..current, first: Some(value), first_finished: True)
        Error(reason) ->
          AsyncJoin2State(
            ..current,
            first_finished: True,
            error: first_join_error(current.error, reason),
          )
      }
      write_ref(state, next)
      async_join2_finish(state)
    }
  }
}

fn async_join2_second(
  state: Ref(AsyncJoin2State(first, second, reason)),
  result: Result(second, reason),
) -> Nil {
  let current = read_ref(state)
  case current.second_finished {
    True -> Nil
    False -> {
      let next = case result {
        Ok(value) ->
          AsyncJoin2State(..current, second: Some(value), second_finished: True)
        Error(reason) ->
          AsyncJoin2State(
            ..current,
            second_finished: True,
            error: first_join_error(current.error, reason),
          )
      }
      write_ref(state, next)
      async_join2_finish(state)
    }
  }
}

fn async_join2_finish(
  state: Ref(AsyncJoin2State(first, second, reason)),
) -> Nil {
  let current = read_ref(state)
  case current.settled {
    True -> Nil
    False ->
      case current.error {
        Some(reason) -> {
          write_ref(state, AsyncJoin2State(..current, settled: True))
          queue_microtask(fn() { current.done(Error(reason)) })
        }
        None ->
          case current.first, current.second {
            Some(first), Some(second) -> {
              write_ref(state, AsyncJoin2State(..current, settled: True))
              queue_microtask(fn() { current.done(Ok(#(first, second))) })
            }
            _, _ -> Nil
          }
      }
  }
}

fn async_join3_first(
  state: Ref(AsyncJoin3State(first, second, third, reason)),
  result: Result(first, reason),
) -> Nil {
  let current = read_ref(state)
  case current.first_finished {
    True -> Nil
    False -> {
      let next = case result {
        Ok(value) ->
          AsyncJoin3State(..current, first: Some(value), first_finished: True)
        Error(reason) ->
          AsyncJoin3State(
            ..current,
            first_finished: True,
            error: first_join_error(current.error, reason),
          )
      }
      write_ref(state, next)
      async_join3_finish(state)
    }
  }
}

fn async_join3_second(
  state: Ref(AsyncJoin3State(first, second, third, reason)),
  result: Result(second, reason),
) -> Nil {
  let current = read_ref(state)
  case current.second_finished {
    True -> Nil
    False -> {
      let next = case result {
        Ok(value) ->
          AsyncJoin3State(..current, second: Some(value), second_finished: True)
        Error(reason) ->
          AsyncJoin3State(
            ..current,
            second_finished: True,
            error: first_join_error(current.error, reason),
          )
      }
      write_ref(state, next)
      async_join3_finish(state)
    }
  }
}

fn async_join3_third(
  state: Ref(AsyncJoin3State(first, second, third, reason)),
  result: Result(third, reason),
) -> Nil {
  let current = read_ref(state)
  case current.third_finished {
    True -> Nil
    False -> {
      let next = case result {
        Ok(value) ->
          AsyncJoin3State(..current, third: Some(value), third_finished: True)
        Error(reason) ->
          AsyncJoin3State(
            ..current,
            third_finished: True,
            error: first_join_error(current.error, reason),
          )
      }
      write_ref(state, next)
      async_join3_finish(state)
    }
  }
}

fn async_join3_finish(
  state: Ref(AsyncJoin3State(first, second, third, reason)),
) -> Nil {
  let current = read_ref(state)
  case current.settled {
    True -> Nil
    False ->
      case current.error {
        Some(reason) -> {
          write_ref(state, AsyncJoin3State(..current, settled: True))
          queue_microtask(fn() { current.done(Error(reason)) })
        }
        None ->
          case current.first, current.second, current.third {
            Some(first), Some(second), Some(third) -> {
              write_ref(state, AsyncJoin3State(..current, settled: True))
              queue_microtask(fn() { current.done(Ok(#(first, second, third))) })
            }
            _, _, _ -> Nil
          }
      }
  }
}

fn first_join_error(error: Option(reason), reason: reason) -> Option(reason) {
  case error {
    Some(error) -> Some(error)
    None -> Some(reason)
  }
}

pub fn async_pure(value: value) -> AsyncComputation(value, reason) {
  AsyncComputation(
    update: fn(_epoch_id, done) { done(Ok(value)) },
    subscribe: fn(_callback) { empty_async_subscription() },
  )
}

pub fn async_map(
  computation: AsyncComputation(input, reason),
  transform: fn(input) -> Async(output, reason),
) -> AsyncComputation(output, reason) {
  let AsyncComputation(update, subscribe) = computation
  let state: AsyncMapState(input, output, reason) =
    AsyncMapState(
      iteration: new_ref(None),
      error: new_ref(None),
      active_epoch: new_ref(None),
      pending: new_ref([]),
      same_dependency: fn(first, second) { same_value(first, second) },
      subscribers: new_ref([]),
      subscriber_ids: new_ref(0),
      dependency_subscription: new_ref(None),
    )
  AsyncComputation(
    update: fn(epoch_id, done) {
      async_map_update(state, update, transform, epoch_id, done)
    },
    subscribe: fn(callback) {
      async_map_subscribe(state, subscribe, update, transform, callback)
    },
  )
}

pub fn async_map2(
  first: AsyncComputation(first, reason),
  second: AsyncComputation(second, reason),
  transform: fn(first, second) -> Async(output, reason),
) -> AsyncComputation(output, reason) {
  let AsyncComputation(update_first, subscribe_first) = first
  let AsyncComputation(update_second, subscribe_second) = second
  let state: AsyncMapState(#(first, second), output, reason) =
    AsyncMapState(
      iteration: new_ref(None),
      error: new_ref(None),
      active_epoch: new_ref(None),
      pending: new_ref([]),
      same_dependency: fn(left: #(first, second), right: #(first, second)) {
        same_value(left.0, right.0) && same_value(left.1, right.1)
      },
      subscribers: new_ref([]),
      subscriber_ids: new_ref(0),
      dependency_subscription: new_ref(None),
    )
  let dependency_update = fn(epoch_id, done) {
    async_all2(update_first, update_second, epoch_id, done)
  }
  let transform_pair = fn(values) {
    let #(first, second) = values
    transform(first, second)
  }
  AsyncComputation(
    update: fn(epoch_id, done) {
      async_map_update(state, dependency_update, transform_pair, epoch_id, done)
    },
    subscribe: fn(callback) {
      // The joined dependency has no public node of its own. Its lifetime is
      // represented by the map's dependency update; this keeps the actual
      // two-input comparison in the cache above.
      async_map_subscribe(
        state,
        fn(callback) {
          let first_subscription = subscribe_first(callback)
          let second_subscription = subscribe_second(callback)
          AsyncSubscription(
            cancel: fn() {
              async_unsubscribe(first_subscription)
              async_unsubscribe(second_subscription)
            },
            keep_alive: AsyncKeepAliveNone,
          )
        },
        dependency_update,
        transform_pair,
        callback,
      )
    },
  )
}

pub fn async_map3(
  first: AsyncComputation(first, reason),
  second: AsyncComputation(second, reason),
  third: AsyncComputation(third, reason),
  transform: fn(first, second, third) -> Async(output, reason),
) -> AsyncComputation(output, reason) {
  let AsyncComputation(update_first, subscribe_first) = first
  let AsyncComputation(update_second, subscribe_second) = second
  let AsyncComputation(update_third, subscribe_third) = third
  let state: AsyncMapState(#(first, second, third), output, reason) =
    AsyncMapState(
      iteration: new_ref(None),
      error: new_ref(None),
      active_epoch: new_ref(None),
      pending: new_ref([]),
      same_dependency: fn(
        left: #(first, second, third),
        right: #(first, second, third),
      ) {
        same_value(left.0, right.0)
        && same_value(left.1, right.1)
        && same_value(left.2, right.2)
      },
      subscribers: new_ref([]),
      subscriber_ids: new_ref(0),
      dependency_subscription: new_ref(None),
    )
  let dependency_update = fn(epoch_id, done) {
    async_all3(update_first, update_second, update_third, epoch_id, done)
  }
  let transform_triple = fn(values) {
    let #(first, second, third) = values
    transform(first, second, third)
  }
  AsyncComputation(
    update: fn(epoch_id, done) {
      async_map_update(
        state,
        dependency_update,
        transform_triple,
        epoch_id,
        done,
      )
    },
    subscribe: fn(callback) {
      async_map_subscribe(
        state,
        fn(callback) {
          let first_subscription = subscribe_first(callback)
          let second_subscription = subscribe_second(callback)
          let third_subscription = subscribe_third(callback)
          AsyncSubscription(
            cancel: fn() {
              async_unsubscribe(first_subscription)
              async_unsubscribe(second_subscription)
              async_unsubscribe(third_subscription)
            },
            keep_alive: AsyncKeepAliveNone,
          )
        },
        dependency_update,
        transform_triple,
        callback,
      )
    },
  )
}

pub fn async_derived1(
  dependency: AsyncComputation(input, reason),
  get: fn(input) -> Async(output, reason),
  set: fn(AsyncContext(reason), output, input) -> Async(Nil, reason),
) -> AsyncDerived1Mutation(input, output, reason) {
  AsyncDerived1Mutation(
    computation: async_map(dependency, get),
    dependency: dependency,
    set: set,
  )
}

pub fn async_derived1_computation(
  derived: AsyncDerived1Mutation(input, output, reason),
) -> AsyncComputation(output, reason) {
  let AsyncDerived1Mutation(computation, ..) = derived
  computation
}

pub fn async_set_derived1(
  context: AsyncContext(reason),
  derived: AsyncDerived1Mutation(input, output, reason),
  value: output,
) -> Async(Nil, reason) {
  let AsyncDerived1Mutation(_, dependency, set) = derived
  let AsyncComputation(update, _) = dependency
  fn(done) {
    update(async_context_epoch(context), fn(result) {
      case result {
        Error(reason) -> done(Error(reason))
        Ok(input) -> set(context, value, input)(done)
      }
    })
  }
}

pub fn async_derived2(
  first: AsyncComputation(first, reason),
  second: AsyncComputation(second, reason),
  get: fn(first, second) -> Async(output, reason),
  set: fn(AsyncContext(reason), output, first, second) -> Async(Nil, reason),
) -> AsyncDerived2Mutation(first, second, output, reason) {
  AsyncDerived2Mutation(
    computation: async_map2(first, second, get),
    first: first,
    second: second,
    set: set,
  )
}

pub fn async_derived2_computation(
  derived: AsyncDerived2Mutation(first, second, output, reason),
) -> AsyncComputation(output, reason) {
  let AsyncDerived2Mutation(computation, ..) = derived
  computation
}

pub fn async_set_derived2(
  context: AsyncContext(reason),
  derived: AsyncDerived2Mutation(first, second, output, reason),
  value: output,
) -> Async(Nil, reason) {
  let AsyncDerived2Mutation(_, first, second, set) = derived
  let AsyncComputation(update_first, _) = first
  let AsyncComputation(update_second, _) = second
  fn(done) {
    async_all2(
      update_first,
      update_second,
      async_context_epoch(context),
      fn(result) {
        case result {
          Error(reason) -> done(Error(reason))
          Ok(#(first, second)) -> set(context, value, first, second)(done)
        }
      },
    )
  }
}

fn async_map_update(
  state: AsyncMapState(input, output, reason),
  dependency_update: fn(Int, fn(Result(input, reason)) -> Nil) -> Nil,
  transform: fn(input) -> Async(output, reason),
  epoch_id: Int,
  done: fn(Result(output, reason)) -> Nil,
) -> Nil {
  case read_ref(state.error) {
    // The source MapImpl retains its rejected iteration Promise. A later
    // epoch therefore observes the same rejection instead of replacing the
    // failed iteration with a new transform.
    Some(#(_error_epoch, reason)) -> done(Error(reason))
    _ ->
      // MapImpl waits for an in-flight iteration before applying its cached
      // epoch check. Returning the previous value first would let a read at
      // epoch 0 observe iteration `a` while the current epoch's `b` update
      // was already running.
      case read_ref(state.active_epoch) {
        Some(_) -> {
          let pending = read_ref(state.pending)
          write_ref(state.pending, list.append(pending, [#(epoch_id, done)]))
        }
        None ->
          case read_ref(state.iteration) {
            Some(AsyncIteration(previous_epoch, _, previous_output))
              if previous_epoch >= epoch_id
            -> done(Ok(previous_output))
            _ ->
              async_map_start(state, dependency_update, transform, epoch_id, [
                #(epoch_id, done),
              ])
          }
      }
  }
}

fn async_map_start(
  state: AsyncMapState(input, output, reason),
  dependency_update: fn(Int, fn(Result(input, reason)) -> Nil) -> Nil,
  transform: fn(input) -> Async(output, reason),
  epoch_id: Int,
  pending: List(#(Int, fn(Result(output, reason)) -> Nil)),
) -> Nil {
  write_ref(state.active_epoch, Some(epoch_id))
  write_ref(state.pending, pending)
  dependency_update(epoch_id, fn(dependency_result) {
    case dependency_result {
      Error(reason) ->
        async_map_complete_error(
          state,
          dependency_update,
          transform,
          epoch_id,
          reason,
        )
      Ok(dependency) -> {
        case read_ref(state.iteration) {
          Some(AsyncIteration(_, previous_dependency, previous_output)) ->
            case state.same_dependency(previous_dependency, dependency) {
              True ->
                async_map_complete(
                  state,
                  dependency_update,
                  transform,
                  epoch_id,
                  dependency,
                  Ok(previous_output),
                )
              False -> {
                transform(dependency)(fn(result) {
                  async_map_complete(
                    state,
                    dependency_update,
                    transform,
                    epoch_id,
                    dependency,
                    result,
                  )
                })
              }
            }
          _ -> {
            transform(dependency)(fn(result) {
              async_map_complete(
                state,
                dependency_update,
                transform,
                epoch_id,
                dependency,
                result,
              )
            })
          }
        }
      }
    }
  })
}

fn async_map_subscribe(
  state: AsyncMapState(input, output, reason),
  dependency_subscribe: fn(fn(Int, fn(Result(Nil, reason)) -> Nil) -> Nil) ->
    AsyncSubscription(reason),
  dependency_update: fn(Int, fn(Result(input, reason)) -> Nil) -> Nil,
  transform: fn(input) -> Async(output, reason),
  callback: fn(Int, fn(Result(Nil, reason)) -> Nil) -> Nil,
) -> AsyncSubscription(reason) {
  let AsyncMapState(subscriber_ids: subscriber_ids, ..) = state
  let subscribers = live_async_subscribers(read_ref(state.subscribers))
  let id = next_id(subscriber_ids)
  let weak_callback = new_weak(callback)
  write_ref(state.subscribers, [
    AsyncSubscriber(id, weak_callback),
    ..subscribers
  ])
  case subscribers {
    [] -> {
      let dependency_subscription =
        dependency_subscribe(fn(epoch_id, done) {
          async_map_update(
            state,
            dependency_update,
            transform,
            epoch_id,
            fn(result) {
              case result {
                Ok(_) -> done(Ok(Nil))
                Error(reason) -> done(Error(reason))
              }
            },
          )
        })
      write_ref(state.dependency_subscription, Some(dependency_subscription))
    }
    _ -> Nil
  }
  AsyncSubscription(
    cancel: fn() { async_map_unsubscribe(state, id) },
    keep_alive: AsyncKeepAlive(callback),
  )
}

fn async_map_unsubscribe(
  state: AsyncMapState(input, output, reason),
  id: Int,
) -> Nil {
  let subscribers =
    read_ref(state.subscribers)
    |> live_async_subscribers
    |> remove_async_subscriber(id)
  write_ref(state.subscribers, subscribers)
  case subscribers {
    [] -> {
      case read_ref(state.dependency_subscription) {
        None -> Nil
        Some(subscription) -> {
          async_unsubscribe(subscription)
          write_ref(state.dependency_subscription, None)
        }
      }
    }
    _ -> Nil
  }
}

fn remove_async_subscriber(
  subscribers: List(AsyncSubscriber(reason)),
  id: Int,
) -> List(AsyncSubscriber(reason)) {
  case subscribers {
    [] -> []
    [AsyncSubscriber(subscriber_id, callback), ..rest] ->
      case subscriber_id == id {
        True -> rest
        False -> [
          AsyncSubscriber(subscriber_id, callback),
          ..remove_async_subscriber(rest, id)
        ]
      }
  }
}

fn async_map_complete(
  state: AsyncMapState(input, output, reason),
  dependency_update: fn(Int, fn(Result(input, reason)) -> Nil) -> Nil,
  transform: fn(input) -> Async(output, reason),
  epoch_id: Int,
  dependency: input,
  result: Result(output, reason),
) -> Nil {
  let previous_iteration = read_ref(state.iteration)
  let output_changed = case result, previous_iteration {
    Ok(output), Some(AsyncIteration(_, _, previous_output)) ->
      !same_value(output, previous_output)
    Ok(_), None -> True
    Error(_), _ -> False
  }
  case result {
    Ok(output) -> {
      write_ref(
        state.iteration,
        Some(AsyncIteration(epoch_id, dependency, output)),
      )
      write_ref(state.error, None)
      let pending = read_ref(state.pending)
      let #(ready, waiting) = split_async_pending(pending, epoch_id, [], [])
      write_ref(state.pending, waiting)
      write_ref(state.active_epoch, None)
      let subscribers = live_async_subscribers(read_ref(state.subscribers))
      write_ref(state.subscribers, subscribers)
      let finish = fn(notification_result) {
        notify_async_map(
          ready,
          first_async_result(Ok(output), notification_result),
        )
        async_map_start_next(state, dependency_update, transform)
      }
      case output_changed {
        True -> notify_async_subscribers(subscribers, epoch_id, Ok(Nil), finish)
        False -> finish(Ok(Nil))
      }
    }
    Error(reason) -> {
      // The source stores the rejected iteration Promise itself. Every caller
      // waiting on that iteration therefore receives the same error, including
      // callers for a later requested epoch; no queued recovery iteration is
      // started after rejection.
      write_ref(state.error, Some(#(epoch_id, reason)))
      let pending = read_ref(state.pending)
      write_ref(state.pending, [])
      write_ref(state.active_epoch, None)
      let subscribers = live_async_subscribers(read_ref(state.subscribers))
      write_ref(state.subscribers, subscribers)
      notify_async_subscribers(
        subscribers,
        epoch_id,
        Error(reason),
        fn(notification_result) {
          notify_async_map(
            pending,
            first_async_result(Error(reason), notification_result),
          )
        },
      )
    }
  }
}

fn async_map_complete_error(
  state: AsyncMapState(input, output, reason),
  _dependency_update: fn(Int, fn(Result(input, reason)) -> Nil) -> Nil,
  _transform: fn(input) -> Async(output, reason),
  epoch_id: Int,
  reason: reason,
) -> Nil {
  write_ref(state.error, Some(#(epoch_id, reason)))
  let pending = read_ref(state.pending)
  write_ref(state.pending, [])
  write_ref(state.active_epoch, None)
  let subscribers = live_async_subscribers(read_ref(state.subscribers))
  write_ref(state.subscribers, subscribers)
  notify_async_subscribers(
    subscribers,
    epoch_id,
    Error(reason),
    fn(notification_result) {
      notify_async_map(
        pending,
        first_async_result(Error(reason), notification_result),
      )
    },
  )
}

fn first_async_result(
  first: Result(output, reason),
  second: Result(Nil, reason),
) -> Result(output, reason) {
  case first {
    Error(_) -> first
    Ok(_) ->
      case second {
        Error(reason) -> Error(reason)
        Ok(_) -> first
      }
  }
}

fn notify_async_map(
  pending: List(#(Int, fn(Result(output, reason)) -> Nil)),
  result: Result(output, reason),
) -> Nil {
  case pending {
    [] -> Nil
    [#(_, done), ..rest] -> {
      done(result)
      notify_async_map(rest, result)
    }
  }
}

fn split_async_pending(
  pending: List(#(Int, fn(Result(output, reason)) -> Nil)),
  epoch_id: Int,
  ready: List(#(Int, fn(Result(output, reason)) -> Nil)),
  waiting: List(#(Int, fn(Result(output, reason)) -> Nil)),
) -> #(
  List(#(Int, fn(Result(output, reason)) -> Nil)),
  List(#(Int, fn(Result(output, reason)) -> Nil)),
) {
  case pending {
    [] -> #(ready, waiting)
    [#(requested_epoch, _callback) as entry, ..rest] ->
      case requested_epoch <= epoch_id {
        True ->
          split_async_pending(
            rest,
            epoch_id,
            list.append(ready, [entry]),
            waiting,
          )
        False ->
          split_async_pending(
            rest,
            epoch_id,
            ready,
            list.append(waiting, [entry]),
          )
      }
  }
}

fn async_map_start_next(
  state: AsyncMapState(input, output, reason),
  dependency_update: fn(Int, fn(Result(input, reason)) -> Nil) -> Nil,
  transform: fn(input) -> Async(output, reason),
) -> Nil {
  case read_ref(state.active_epoch), read_ref(state.pending) {
    Some(_), _ -> Nil
    None, [] -> Nil
    None, pending -> {
      let epoch_id = max_async_pending_epoch(pending, 0)
      async_map_start(state, dependency_update, transform, epoch_id, pending)
    }
  }
}

fn max_async_pending_epoch(
  pending: List(#(Int, fn(Result(output, reason)) -> Nil)),
  current: Int,
) -> Int {
  case pending {
    [] -> current
    [#(epoch_id, _), ..rest] ->
      max_async_pending_epoch(rest, max_int(epoch_id, current))
  }
}

fn max_int(first: Int, second: Int) -> Int {
  case first >= second {
    True -> first
    False -> second
  }
}

fn notify_async_subscribers(
  subscribers: List(AsyncSubscriber(reason)),
  epoch_id: Int,
  result: Result(Nil, reason),
  done: fn(Result(Nil, reason)) -> Nil,
) -> Nil {
  case subscribers {
    [] -> done(result)
    [AsyncSubscriber(_, callback), ..rest] ->
      case deref_weak(callback) {
        None -> notify_async_subscribers(rest, epoch_id, result, done)
        Some(callback) ->
          callback(epoch_id, fn(callback_result) {
            notify_async_subscribers(
              rest,
              epoch_id,
              first_async_error(result, callback_result),
              done,
            )
          })
      }
  }
}

fn first_async_error(
  first: Result(Nil, reason),
  second: Result(Nil, reason),
) -> Result(Nil, reason) {
  case first {
    Error(_) -> first
    Ok(_) -> second
  }
}

fn live_async_subscribers(
  subscribers: List(AsyncSubscriber(reason)),
) -> List(AsyncSubscriber(reason)) {
  case subscribers {
    [] -> []
    [AsyncSubscriber(id, callback), ..rest] ->
      case deref_weak(callback) {
        None -> live_async_subscribers(rest)
        Some(_) -> [
          AsyncSubscriber(id, callback),
          ..live_async_subscribers(rest)
        ]
      }
  }
}

pub fn async_value(
  computation: AsyncComputation(value, reason),
  done: fn(Result(value, reason)) -> Nil,
) -> Nil {
  let AsyncComputation(update, _) = computation
  update(0, done)
}

pub fn async_context() -> AsyncContext(reason) {
  AsyncContext(
    next_epoch: new_ref(0),
    transactions: new_ref([]),
    in_progress_epoch: new_ref(None),
    in_progress_count: new_ref(0),
  )
}

fn async_context_epoch(context: AsyncContext(reason)) -> Int {
  let AsyncContext(in_progress_epoch: epoch_ref, ..) = context
  case read_ref(epoch_ref) {
    Some(epoch_id) -> epoch_id
    None -> 0
  }
}

pub fn async_state(initial: value) -> AsyncMutation(value, reason) {
  let state =
    AsyncState(
      value: new_ref(initial),
      epoch: new_ref(0),
      updated: new_ref(True),
      subscribers: new_ref([]),
      subscriber_ids: new_ref(0),
    )
  let computation =
    AsyncComputation(
      update: fn(epoch_id, done) { async_state_update(state, epoch_id, done) },
      subscribe: fn(callback) { async_state_subscribe(state, callback) },
    )
  AsyncMutation(computation: computation, set: fn(context, value) {
    async_state_set(context, state, value)
  })
}

pub fn async_mutation(
  mutation: AsyncMutation(value, reason),
) -> AsyncComputation(value, reason) {
  let AsyncMutation(computation, _) = mutation
  computation
}

pub fn async_set(
  context: AsyncContext(reason),
  mutation: AsyncMutation(value, reason),
  value: value,
) -> Result(Nil, DataflowError) {
  let AsyncMutation(_, set) = mutation
  set(context, value)
}

pub fn async_txn(
  context: AsyncContext(reason),
  callback: fn() -> Async(output, reason),
) -> Async(output, reason) {
  fn(done) {
    async_begin_transaction(context)
    callback()(fn(result) {
      case result {
        Error(reason) -> {
          async_rollback_transaction(context)
          done(Error(reason))
        }
        Ok(output) ->
          async_commit_transaction(context, fn(commit_result) {
            case commit_result {
              Ok(_) -> done(Ok(output))
              Error(reason) -> done(Error(reason))
            }
          })
      }
    })
  }
}

fn async_state_update(
  state: AsyncState(value, reason),
  epoch_id: Int,
  done: fn(Result(value, reason)) -> Nil,
) -> Nil {
  let value = read_ref(state.value)
  case !read_ref(state.updated) && epoch_id >= read_ref(state.epoch) {
    False -> done(Ok(value))
    True -> {
      // Mark the state observed before notifying children. An effect's
      // dependency read therefore sees the committed value without re-entering
      // the same subscriber list.
      write_ref(state.updated, True)
      let subscribers = live_async_subscribers(read_ref(state.subscribers))
      write_ref(state.subscribers, subscribers)
      // The subscriber continuation owns the asynchronous boundary, so start
      // it directly and let the outer epoch decide whether to wait for it.
      notify_async_subscribers(subscribers, epoch_id, Ok(Nil), fn(result) {
        case result {
          Ok(_) -> done(Ok(value))
          Error(reason) -> done(Error(reason))
        }
      })
    }
  }
}

fn async_state_subscribe(
  state: AsyncState(value, reason),
  callback: fn(Int, fn(Result(Nil, reason)) -> Nil) -> Nil,
) -> AsyncSubscription(reason) {
  let AsyncState(subscriber_ids: subscriber_ids, ..) = state
  let id = next_id(subscriber_ids)
  let subscribers = live_async_subscribers(read_ref(state.subscribers))
  let weak_callback = new_weak(callback)
  write_ref(state.subscribers, [
    AsyncSubscriber(id, weak_callback),
    ..subscribers
  ])
  AsyncSubscription(
    cancel: fn() {
      write_ref(
        state.subscribers,
        remove_async_subscriber(read_ref(state.subscribers), id),
      )
    },
    keep_alive: AsyncKeepAlive(callback),
  )
}

fn async_state_set(
  context: AsyncContext(reason),
  state: AsyncState(value, reason),
  value: value,
) -> Result(Nil, DataflowError) {
  let AsyncContext(transactions: transactions_ref, ..) = context
  case read_ref(transactions_ref) {
    [] -> Error(TransactionRequired)
    [transaction, ..rest] -> {
      let operation =
        AsyncOperation(
          commit: fn(epoch_id) { async_commit_state(state, value, epoch_id) },
          update: fn(epoch_id, done) {
            async_state_update(state, epoch_id, fn(result) {
              case result {
                Ok(_) -> done(Ok(Nil))
                Error(reason) -> done(Error(reason))
              }
            })
          },
        )
      write_ref(transactions_ref, [
        list.append(transaction, [operation]),
        ..rest
      ])
      Ok(Nil)
    }
  }
}

fn async_commit_state(
  state: AsyncState(value, reason),
  value: value,
  epoch_id: Int,
) -> Nil {
  case same_value(read_ref(state.value), value) {
    True -> Nil
    False -> {
      write_ref(state.value, value)
      write_ref(state.epoch, epoch_id)
      write_ref(state.updated, False)
    }
  }
}

fn async_begin_transaction(context: AsyncContext(reason)) -> Nil {
  let AsyncContext(transactions: transactions_ref, ..) = context
  write_ref(transactions_ref, [[], ..read_ref(transactions_ref)])
}

fn async_rollback_transaction(context: AsyncContext(reason)) -> Nil {
  let AsyncContext(transactions: transactions_ref, ..) = context
  case read_ref(transactions_ref) {
    [] -> Nil
    [_transaction, ..rest] -> write_ref(transactions_ref, rest)
  }
}

fn async_commit_transaction(
  context: AsyncContext(reason),
  done: fn(Result(Nil, reason)) -> Nil,
) -> Nil {
  let AsyncContext(
    next_epoch: next_epoch_ref,
    transactions: transactions_ref,
    in_progress_epoch: in_progress_epoch_ref,
    in_progress_count: in_progress_count_ref,
  ) = context
  case read_ref(transactions_ref) {
    [] -> done(Ok(Nil))
    [transaction, ..rest] ->
      case rest {
        [parent, ..parents] -> {
          write_ref(transactions_ref, [
            list.append(parent, transaction),
            ..parents
          ])
          done(Ok(Nil))
        }
        [] -> {
          // DataflowImpl reuses its current Epoch while that epoch still has
          // asynchronous graph work in progress. A nested transaction must
          // therefore commit into the same epoch; allocating a new integer
          // here makes a derived graph recompute a value that the source
          // deliberately serves from the current iteration.
          let #(epoch_id, owns_epoch) = case read_ref(in_progress_epoch_ref) {
            Some(epoch_id) -> #(epoch_id, False)
            None -> {
              let epoch_id = read_ref(next_epoch_ref) + 1
              write_ref(next_epoch_ref, epoch_id)
              write_ref(in_progress_epoch_ref, Some(epoch_id))
              #(epoch_id, True)
            }
          }
          write_ref(transactions_ref, [])
          write_ref(in_progress_count_ref, read_ref(in_progress_count_ref) + 1)
          async_commit_all(transaction, epoch_id)
          case owns_epoch {
            True ->
              async_update_all(transaction, epoch_id, fn(result) {
                let count = read_ref(in_progress_count_ref) - 1
                write_ref(in_progress_count_ref, count)
                case count {
                  0 -> write_ref(in_progress_epoch_ref, None)
                  _ -> Nil
                }
                done(result)
              })
            False -> {
              // The source's inner commit does not wait for the already
              // active epoch. Start its state update so the graph sees the
              // committed value, but release the nested transaction before
              // the asynchronous subscriber callbacks complete. The outer
              // owner of the epoch is responsible for waiting on that work.
              async_update_all(transaction, epoch_id, fn(_result) {
                let count = read_ref(in_progress_count_ref) - 1
                write_ref(in_progress_count_ref, count)
                case count {
                  0 -> write_ref(in_progress_epoch_ref, None)
                  _ -> Nil
                }
              })
              done(Ok(Nil))
            }
          }
        }
      }
  }
}

fn async_commit_all(
  operations: List(AsyncOperation(reason)),
  epoch_id: Int,
) -> Nil {
  case operations {
    [] -> Nil
    [AsyncOperation(commit, _), ..rest] -> {
      commit(epoch_id)
      async_commit_all(rest, epoch_id)
    }
  }
}

fn async_update_all(
  operations: List(AsyncOperation(reason)),
  epoch_id: Int,
  done: fn(Result(Nil, reason)) -> Nil,
) -> Nil {
  case operations {
    [] -> done(Ok(Nil))
    [AsyncOperation(_, update), ..rest] ->
      update(epoch_id, fn(result) {
        case result {
          Error(reason) -> done(Error(reason))
          Ok(_) -> async_update_all(rest, epoch_id, done)
        }
      })
  }
}

// The source effect awaits its first update and callback before linking the
// effect into the dependency graph. Returning the subscription as an Async
// value preserves that ordering without inventing a Promise type in Gleam.
pub fn async_effect(
  computation: AsyncComputation(value, reason),
  callback: fn(value) -> Async(Nil, reason),
) -> Async(AsyncSubscription(reason), reason) {
  let AsyncComputation(update, subscribe) = computation
  let error = new_ref(None)
  fn(done) {
    update(0, fn(result) {
      case result {
        Error(reason) -> {
          write_ref(error, Some(reason))
          done(Error(reason))
        }
        Ok(value) ->
          callback(value)(fn(callback_result) {
            case callback_result {
              Error(reason) -> {
                write_ref(error, Some(reason))
                done(Error(reason))
              }
              Ok(_) -> {
                let subscription =
                  subscribe(fn(epoch_id, effect_done) {
                    case read_ref(error) {
                      Some(reason) -> effect_done(Error(reason))
                      None ->
                        update(epoch_id, fn(update_result) {
                          case update_result {
                            Error(reason) -> {
                              write_ref(error, Some(reason))
                              effect_done(Error(reason))
                            }
                            Ok(value) ->
                              callback(value)(fn(callback_result) {
                                case callback_result {
                                  Error(reason) -> {
                                    write_ref(error, Some(reason))
                                    effect_done(Error(reason))
                                  }
                                  Ok(_) -> effect_done(Ok(Nil))
                                }
                              })
                          }
                        })
                    }
                  })
                done(Ok(subscription))
              }
            }
          })
      }
    })
  }
}

pub fn async_effect2(
  first: AsyncComputation(first, reason),
  second: AsyncComputation(second, reason),
  callback: fn(first, second) -> Async(Nil, reason),
) -> Async(AsyncSubscription(reason), reason) {
  let computation =
    async_map2(first, second, fn(first, second) {
      immediate_async(#(first, second))
    })
  async_effect(computation, fn(values) { callback(values.0, values.1) })
}

pub fn async_effect3(
  first: AsyncComputation(first, reason),
  second: AsyncComputation(second, reason),
  third: AsyncComputation(third, reason),
  callback: fn(first, second, third) -> Async(Nil, reason),
) -> Async(AsyncSubscription(reason), reason) {
  let computation =
    async_map3(first, second, third, fn(first, second, third) {
      immediate_async(#(first, second, third))
    })
  async_effect(computation, fn(values) {
    callback(values.0, values.1, values.2)
  })
}

pub fn handler_async(
  callback: fn(event, input) -> Async(output, reason),
  input: NodeOpt(input),
) -> fn(event) -> Async(output, reason) {
  let computation = to_computation(input)
  fn(event) { callback(event, value(computation)) }
}

pub fn handler_async2(
  callback: fn(event, first, second) -> Async(output, reason),
  first: NodeOpt(first),
  second: NodeOpt(second),
) -> fn(event) -> Async(output, reason) {
  let first = to_computation(first)
  let second = to_computation(second)
  fn(event) { callback(event, value(first), value(second)) }
}

fn immediate_async(value: value) -> Async(value, reason) {
  fn(done) { done(Ok(value)) }
}

@external(javascript, "./dataflow_ref_ffi.mjs", "new_ref")
fn new_ref(value: value) -> Ref(value)

@external(javascript, "./async_ffi.mjs", "queue_microtask")
fn queue_microtask(callback: fn() -> Nil) -> Nil

@external(javascript, "./dataflow_ref_ffi.mjs", "new_weak")
fn new_weak(value: value) -> Weak(value)

@external(javascript, "./dataflow_ref_ffi.mjs", "deref_weak")
fn deref_weak(reference: Weak(value)) -> Option(value)

@external(javascript, "./dataflow_ref_ffi.mjs", "read_ref")
fn read_ref(reference: Ref(value)) -> value

@external(javascript, "./dataflow_ref_ffi.mjs", "write_ref")
fn write_ref(reference: Ref(value), value: value) -> Nil

@external(javascript, "./dataflow_ref_ffi.mjs", "same_value")
fn same_value(first: value, second: value) -> Bool

fn next_id(reference: Ref(Int)) -> Int {
  let id = read_ref(reference)
  write_ref(reference, id + 1)
  id
}

pub fn dataflow() -> Context {
  Context(scheduler: Scheduler(
    transactions: new_ref([]),
    queue: new_ref([]),
    queued: new_ref([]),
  ))
}

pub fn literal(value: value) -> NodeOpt(value) {
  Literal(value)
}

pub fn reactive(computation: Computation(value)) -> NodeOpt(value) {
  Reactive(computation)
}

pub fn mutation(mutation: Mutation(value)) -> NodeOpt(value) {
  let Mutation(computation, _) = mutation
  Reactive(computation)
}

pub fn pure(value: value) -> Computation(value) {
  Computation(
    read: fn() { value },
    revision: fn() { 0 },
    subscribe: fn(_callback) { empty_subscription() },
  )
}

pub fn state(initial: value) -> Mutation(value) {
  let cell =
    Cell(
      value: new_ref(initial),
      revision: new_ref(0),
      subscribers: new_ref([]),
      identity: new_ref(Nil),
      subscriber_ids: new_ref(0),
    )
  let computation = cell_computation(cell)
  Mutation(computation: computation, set: fn(context, value) {
    let Context(scheduler) = context
    case queue_set(scheduler, cell, value) {
      True -> Ok(Nil)
      False -> Error(TransactionRequired)
    }
  })
}

pub fn value(computation: Computation(value)) -> value {
  let Computation(read, _, _) = computation
  read()
}

pub fn to_computation(input: NodeOpt(value)) -> Computation(value) {
  case input {
    Literal(value) -> pure(value)
    Reactive(computation) -> computation
  }
}

pub fn map(
  transform: fn(input) -> output,
  input: NodeOpt(input),
) -> Computation(output) {
  let computation = to_computation(input)
  let memo = new_map_memo()
  let read = fn() {
    let current_value = value(computation)
    case map_memo_matches(memo, current_value) {
      True -> map_memo_read(memo)
      False -> map_memo_write(memo, current_value, transform(current_value))
    }
  }
  Computation(
    read: read,
    revision: fn() { revision(computation) },
    subscribe: fn(callback) {
      subscribe_changed_from(computation, read, callback)
    },
  )
}

// The original implementation exposed these operations through a Proxy
// (`node.x` and `node[0]`). Gleam has no dynamic property access, so the
// migration surface makes the projection explicit and keeps the field type
// checked by the getter supplied at the call site.
pub fn field(
  input: NodeOpt(record),
  get: fn(record) -> value,
) -> Computation(value) {
  map(get, input)
}

pub fn map2(
  transform: fn(first, second) -> output,
  first: NodeOpt(first),
  second: NodeOpt(second),
) -> Computation(output) {
  let first = to_computation(first)
  let second = to_computation(second)
  let memo = new_map2_memo()
  let read = fn() {
    let first_value = value(first)
    let second_value = value(second)
    case map2_memo_matches(memo, first_value, second_value) {
      True -> map2_memo_read(memo)
      False ->
        map2_memo_write(
          memo,
          first_value,
          second_value,
          transform(first_value, second_value),
        )
    }
  }
  Computation(
    read: read,
    revision: fn() { max_revision(revision(first), revision(second)) },
    subscribe: fn(callback) {
      subscribe_changed_from2(first, second, read, callback)
    },
  )
}

pub fn map3(
  transform: fn(first, second, third) -> output,
  first: NodeOpt(first),
  second: NodeOpt(second),
  third: NodeOpt(third),
) -> Computation(output) {
  let first = to_computation(first)
  let second = to_computation(second)
  let third = to_computation(third)
  let memo = new_map3_memo()
  let read = fn() {
    let first_value = value(first)
    let second_value = value(second)
    let third_value = value(third)
    case map3_memo_matches(memo, first_value, second_value, third_value) {
      True -> map3_memo_read(memo)
      False ->
        map3_memo_write(
          memo,
          first_value,
          second_value,
          third_value,
          transform(first_value, second_value, third_value),
        )
    }
  }
  Computation(
    read: read,
    revision: fn() {
      max_revision(
        max_revision(revision(first), revision(second)),
        revision(third),
      )
    },
    subscribe: fn(callback) {
      subscribe_changed_from3(first, second, third, read, callback)
    },
  )
}

pub fn derived(
  get: fn(input) -> output,
  set: fn(Context, output, input) -> Result(Nil, DataflowError),
  input: NodeOpt(input),
) -> Mutation(output) {
  let computation = to_computation(input)
  let read = fn() { get(value(computation)) }
  Mutation(
    computation: Computation(
      read: read,
      revision: fn() { revision(computation) },
      subscribe: fn(callback) {
        subscribe_changed_from(computation, read, callback)
      },
    ),
    set: fn(context, new_value) { set(context, new_value, value(computation)) },
  )
}

pub fn field_mutation(
  input: Mutation(record),
  get: fn(record) -> value,
  update: fn(Context, value, record) -> Result(Nil, DataflowError),
) -> Mutation(value) {
  derived(get, update, mutation(input))
}

pub fn derived2(
  get: fn(first, second) -> output,
  set: fn(Context, output, first, second) -> Result(Nil, DataflowError),
  first: NodeOpt(first),
  second: NodeOpt(second),
) -> Mutation(output) {
  let first = to_computation(first)
  let second = to_computation(second)
  let read = fn() { get(value(first), value(second)) }
  Mutation(
    computation: Computation(
      read: read,
      revision: fn() { max_revision(revision(first), revision(second)) },
      subscribe: fn(callback) {
        subscribe_changed_from2(first, second, read, callback)
      },
    ),
    set: fn(context, new_value) {
      set(context, new_value, value(first), value(second))
    },
  )
}

pub fn effect(
  callback: fn(value) -> Nil,
  computation: Computation(value),
) -> Subscription {
  callback(value(computation))
  subscribe(computation, fn() { callback(value(computation)) })
}

// Subscribe to future computation changes without replaying the current
// value. This is useful when a consumer has already applied the initial value
// with context-specific semantics, such as preserving an existing DOM
// attribute at a portal boundary.
pub fn effect_after_initial(
  callback: fn(value) -> Nil,
  computation: Computation(value),
) -> Subscription {
  subscribe(computation, fn() { callback(value(computation)) })
}

pub fn effect2(
  callback: fn(first, second) -> Nil,
  first: Computation(first),
  second: Computation(second),
) -> Subscription {
  effect2_with_initial(first, second, callback, True)
}

pub fn effect2_after_initial(
  callback: fn(first, second) -> Nil,
  first: Computation(first),
  second: Computation(second),
) -> Subscription {
  effect2_with_initial(first, second, callback, False)
}

fn effect2_with_initial(
  first: Computation(first),
  second: Computation(second),
  callback: fn(first, second) -> Nil,
  include_initial: Bool,
) -> Subscription {
  let first_last = new_memo()
  let second_last = new_memo()
  let first_value = value(first)
  let second_value = value(second)
  memo_write(first_last, [0], first_value)
  memo_write(second_last, [0], second_value)
  case include_initial {
    True -> callback(first_value, second_value)
    False -> Nil
  }
  let notify = fn() {
    let next_first = value(first)
    let next_second = value(second)
    let first_changed = !memo_value_matches(first_last, next_first)
    let second_changed = !memo_value_matches(second_last, next_second)
    case first_changed || second_changed {
      True -> {
        memo_write(first_last, [0], next_first)
        memo_write(second_last, [0], next_second)
        callback(next_first, next_second)
      }
      False -> Nil
    }
  }
  let first_subscription = subscribe(first, notify)
  let second_subscription = subscribe(second, notify)
  Subscription(
    cancel: fn() {
      unsubscribe(first_subscription)
      unsubscribe(second_subscription)
    },
    keep_alive: KeepAliveNone,
  )
}

pub fn effect3(
  callback: fn(first, second, third) -> Nil,
  first: Computation(first),
  second: Computation(second),
  third: Computation(third),
) -> Subscription {
  effect3_with_initial(first, second, third, callback, True)
}

pub fn effect3_after_initial(
  callback: fn(first, second, third) -> Nil,
  first: Computation(first),
  second: Computation(second),
  third: Computation(third),
) -> Subscription {
  effect3_with_initial(first, second, third, callback, False)
}

fn effect3_with_initial(
  first: Computation(first),
  second: Computation(second),
  third: Computation(third),
  callback: fn(first, second, third) -> Nil,
  include_initial: Bool,
) -> Subscription {
  let first_last = new_memo()
  let second_last = new_memo()
  let third_last = new_memo()
  let first_value = value(first)
  let second_value = value(second)
  let third_value = value(third)
  memo_write(first_last, [0], first_value)
  memo_write(second_last, [0], second_value)
  memo_write(third_last, [0], third_value)
  case include_initial {
    True -> callback(first_value, second_value, third_value)
    False -> Nil
  }
  let notify = fn() {
    let next_first = value(first)
    let next_second = value(second)
    let next_third = value(third)
    let first_changed = !memo_value_matches(first_last, next_first)
    let second_changed = !memo_value_matches(second_last, next_second)
    let third_changed = !memo_value_matches(third_last, next_third)
    case first_changed || second_changed || third_changed {
      True -> {
        memo_write(first_last, [0], next_first)
        memo_write(second_last, [0], next_second)
        memo_write(third_last, [0], next_third)
        callback(next_first, next_second, next_third)
      }
      False -> Nil
    }
  }
  let first_subscription = subscribe(first, notify)
  let second_subscription = subscribe(second, notify)
  let third_subscription = subscribe(third, notify)
  Subscription(
    cancel: fn() {
      unsubscribe(first_subscription)
      unsubscribe(second_subscription)
      unsubscribe(third_subscription)
    },
    keep_alive: KeepAliveNone,
  )
}

pub fn handler(
  callback: fn(event, input) -> output,
  input: NodeOpt(input),
) -> fn(event) -> output {
  let computation = to_computation(input)
  fn(event) { callback(event, value(computation)) }
}

pub fn handler2(
  callback: fn(event, first, second) -> output,
  first: NodeOpt(first),
  second: NodeOpt(second),
) -> fn(event) -> output {
  let first = to_computation(first)
  let second = to_computation(second)
  fn(event) { callback(event, value(first), value(second)) }
}

pub fn set(
  context: Context,
  mutation: Mutation(value),
  new_value: value,
) -> Result(Nil, DataflowError) {
  let Mutation(_, set) = mutation
  set(context, new_value)
}

pub fn txn(
  context: Context,
  callback: fn() -> Result(output, error),
) -> Result(output, error) {
  let Context(scheduler) = context
  begin_transaction(scheduler)
  case callback() {
    Ok(output) -> {
      commit_transaction(scheduler)
      Ok(output)
    }
    Error(error) -> {
      rollback_transaction(scheduler)
      Error(error)
    }
  }
}

// The source event boundary keeps a dataflow transaction open while an async
// listener is awaited. This continuation form preserves that ordering without
// pretending that a Gleam function can suspend a synchronous callback.
pub fn txn_async(
  context: Context,
  callback: fn() -> Async(output, reason),
) -> Async(output, reason) {
  fn(done) {
    let Context(scheduler) = context
    begin_transaction(scheduler)
    callback()(fn(result) {
      case result {
        Ok(output) -> {
          commit_transaction(scheduler)
          done(Ok(output))
        }
        Error(reason) -> {
          rollback_transaction(scheduler)
          done(Error(reason))
        }
      }
    })
  }
}

pub fn unsubscribe(subscription: Subscription) -> Nil {
  let Subscription(cancel, _) = subscription
  cancel()
}

// A small owner-created subscription is useful when a scoped callback adds
// resources after its initial mount continuation has returned. The owner can
// still release those resources through the same subscription lifecycle as
// ordinary dataflow effects.
pub fn subscription(cancel: fn() -> Nil) -> Subscription {
  Subscription(cancel: cancel, keep_alive: KeepAliveNone)
}

pub fn async_unsubscribe(subscription: AsyncSubscription(reason)) -> Nil {
  let AsyncSubscription(cancel, _) = subscription
  cancel()
}

fn cell_computation(cell: Cell(value)) -> Computation(value) {
  Computation(
    read: fn() { read_cell(cell) },
    revision: fn() { cell_revision(cell) },
    subscribe: fn(callback) {
      let cell_subscription = subscribe_cell(cell, callback)
      Subscription(
        cancel: fn() { unsubscribe_cell(cell_subscription) },
        keep_alive: KeepAliveSync(callback),
      )
    },
  )
}

fn subscribe(
  computation: Computation(value),
  callback: fn() -> Nil,
) -> Subscription {
  let Computation(_, _, subscribe) = computation
  subscribe(callback)
}

fn subscribe_changed_from(
  dependency: Computation(dependency),
  read: fn() -> value,
  callback: fn() -> Nil,
) -> Subscription {
  let last = new_memo()
  memo_write(last, [0], read())
  subscribe(dependency, fn() {
    let next_value = read()
    case memo_value_matches(last, next_value) {
      True -> Nil
      False -> {
        memo_write(last, [0], next_value)
        callback()
      }
    }
  })
}

fn subscribe_changed_from2(
  first: Computation(first),
  second: Computation(second),
  read: fn() -> value,
  callback: fn() -> Nil,
) -> Subscription {
  let last = new_memo()
  memo_write(last, [0], read())
  let notify = fn() {
    let next_value = read()
    case memo_value_matches(last, next_value) {
      True -> Nil
      False -> {
        memo_write(last, [0], next_value)
        callback()
      }
    }
  }
  let first_subscription = subscribe(first, notify)
  let second_subscription = subscribe(second, notify)
  Subscription(
    cancel: fn() {
      unsubscribe(first_subscription)
      unsubscribe(second_subscription)
    },
    keep_alive: KeepAliveNone,
  )
}

fn subscribe_changed_from3(
  first: Computation(first),
  second: Computation(second),
  third: Computation(third),
  read: fn() -> value,
  callback: fn() -> Nil,
) -> Subscription {
  let last = new_memo()
  memo_write(last, [0], read())
  let notify = fn() {
    let next_value = read()
    case memo_value_matches(last, next_value) {
      True -> Nil
      False -> {
        memo_write(last, [0], next_value)
        callback()
      }
    }
  }
  let first_subscription = subscribe(first, notify)
  let second_subscription = subscribe(second, notify)
  let third_subscription = subscribe(third, notify)
  Subscription(
    cancel: fn() {
      unsubscribe(first_subscription)
      unsubscribe(second_subscription)
      unsubscribe(third_subscription)
    },
    keep_alive: KeepAliveNone,
  )
}

fn empty_subscription() -> Subscription {
  Subscription(cancel: fn() { Nil }, keep_alive: KeepAliveNone)
}

fn empty_async_subscription() -> AsyncSubscription(reason) {
  AsyncSubscription(cancel: fn() { Nil }, keep_alive: AsyncKeepAliveNone)
}

fn revision(computation: Computation(value)) -> Int {
  let Computation(_, revision, _) = computation
  revision()
}

fn max_revision(first: Int, second: Int) -> Int {
  case first >= second {
    True -> first
    False -> second
  }
}

fn read_cell(cell: Cell(value)) -> value {
  let Cell(value: value_ref, ..) = cell
  read_ref(value_ref)
}

fn cell_revision(cell: Cell(value)) -> Int {
  let Cell(revision: revision_ref, ..) = cell
  read_ref(revision_ref)
}

fn new_memo() -> Memo(value) {
  Memo(revisions: new_ref(None), value: new_ref(None))
}

fn new_map_memo() -> MapMemo(input, output) {
  MapMemo(input: new_ref(None), output: new_ref(None))
}

fn new_map2_memo() -> Map2Memo(first, second, output) {
  Map2Memo(first: new_ref(None), second: new_ref(None), output: new_ref(None))
}

fn new_map3_memo() -> Map3Memo(first, second, third, output) {
  Map3Memo(
    first: new_ref(None),
    second: new_ref(None),
    third: new_ref(None),
    output: new_ref(None),
  )
}

fn memo_value_matches(memo: Memo(value), value: value) -> Bool {
  let Memo(revisions: revisions_ref, value: value_ref) = memo
  case read_ref(revisions_ref), read_ref(value_ref) {
    Some(_), Some(previous) -> same_value(previous, value)
    _, _ -> False
  }
}

fn memo_write(memo: Memo(value), revisions: List(Int), value: value) -> value {
  let Memo(revisions: revisions_ref, value: value_ref) = memo
  write_ref(revisions_ref, Some(revisions))
  write_ref(value_ref, Some(value))
  value
}

fn map_memo_matches(memo: MapMemo(input, output), input: input) -> Bool {
  let MapMemo(input: input_ref, ..) = memo
  case read_ref(input_ref) {
    Some(previous) -> same_value(previous, input)
    None -> False
  }
}

fn map_memo_read(memo: MapMemo(input, output)) -> output {
  let MapMemo(output: output_ref, ..) = memo
  let assert Some(output) = read_ref(output_ref)
  output
}

fn map_memo_write(
  memo: MapMemo(input, output),
  input: input,
  output: output,
) -> output {
  let MapMemo(input: input_ref, output: output_ref) = memo
  write_ref(input_ref, Some(input))
  write_ref(output_ref, Some(output))
  output
}

fn map2_memo_matches(
  memo: Map2Memo(first, second, output),
  first: first,
  second: second,
) -> Bool {
  let Map2Memo(first: first_ref, second: second_ref, ..) = memo
  case read_ref(first_ref), read_ref(second_ref) {
    Some(previous_first), Some(previous_second) ->
      same_value(previous_first, first) && same_value(previous_second, second)
    _, _ -> False
  }
}

fn map2_memo_read(memo: Map2Memo(first, second, output)) -> output {
  let Map2Memo(output: output_ref, ..) = memo
  let assert Some(output) = read_ref(output_ref)
  output
}

fn map2_memo_write(
  memo: Map2Memo(first, second, output),
  first: first,
  second: second,
  output: output,
) -> output {
  let Map2Memo(first: first_ref, second: second_ref, output: output_ref) = memo
  write_ref(first_ref, Some(first))
  write_ref(second_ref, Some(second))
  write_ref(output_ref, Some(output))
  output
}

fn map3_memo_matches(
  memo: Map3Memo(first, second, third, output),
  first: first,
  second: second,
  third: third,
) -> Bool {
  let Map3Memo(first: first_ref, second: second_ref, third: third_ref, ..) =
    memo
  case read_ref(first_ref), read_ref(second_ref), read_ref(third_ref) {
    Some(previous_first), Some(previous_second), Some(previous_third) ->
      same_value(previous_first, first)
      && same_value(previous_second, second)
      && same_value(previous_third, third)
    _, _, _ -> False
  }
}

fn map3_memo_read(memo: Map3Memo(first, second, third, output)) -> output {
  let Map3Memo(output: output_ref, ..) = memo
  let assert Some(output) = read_ref(output_ref)
  output
}

fn map3_memo_write(
  memo: Map3Memo(first, second, third, output),
  first: first,
  second: second,
  third: third,
  output: output,
) -> output {
  let Map3Memo(
    first: first_ref,
    second: second_ref,
    third: third_ref,
    output: output_ref,
  ) = memo
  write_ref(first_ref, Some(first))
  write_ref(second_ref, Some(second))
  write_ref(third_ref, Some(third))
  write_ref(output_ref, Some(output))
  output
}

fn subscribe_cell(
  cell: Cell(value),
  callback: fn() -> Nil,
) -> CellSubscription(value) {
  let Cell(subscribers: subscribers_ref, subscriber_ids: subscriber_ids, ..) =
    cell
  let id = next_id(subscriber_ids)
  let subscribers = read_ref(subscribers_ref)
  write_ref(
    subscribers_ref,
    append(subscribers, Subscriber(id: id, callback: new_weak(callback))),
  )
  CellSubscription(cell: cell, id: id)
}

fn unsubscribe_cell(subscription: CellSubscription(value)) -> Nil {
  let CellSubscription(cell, id) = subscription
  let Cell(subscribers: subscribers_ref, ..) = cell
  write_ref(subscribers_ref, remove_subscriber(read_ref(subscribers_ref), id))
}

fn remove_subscriber(
  subscribers: List(Subscriber),
  id: Int,
) -> List(Subscriber) {
  case subscribers {
    [] -> []
    [Subscriber(subscriber_id, callback), ..rest] ->
      case subscriber_id == id {
        True -> rest
        False -> [
          Subscriber(subscriber_id, callback),
          ..remove_subscriber(rest, id)
        ]
      }
  }
}

fn begin_transaction(scheduler: Scheduler) -> Nil {
  let Scheduler(transactions: transactions_ref, ..) = scheduler
  write_ref(transactions_ref, [[], ..read_ref(transactions_ref)])
}

fn queue_set(scheduler: Scheduler, cell: Cell(value), value: value) -> Bool {
  let Scheduler(transactions: transactions_ref, ..) = scheduler
  case read_ref(transactions_ref) {
    [] -> False
    [transaction, ..rest] -> {
      let operation = fn() { commit_cell(scheduler, cell, value) }
      write_ref(transactions_ref, [append(transaction, operation), ..rest])
      True
    }
  }
}

fn rollback_transaction(scheduler: Scheduler) -> Nil {
  let Scheduler(transactions: transactions_ref, ..) = scheduler
  case read_ref(transactions_ref) {
    [] -> Nil
    [_transaction, ..rest] -> write_ref(transactions_ref, rest)
  }
}

fn commit_transaction(scheduler: Scheduler) -> Nil {
  let Scheduler(transactions: transactions_ref, ..) = scheduler
  case read_ref(transactions_ref) {
    [] -> Nil
    [transaction, ..rest] ->
      case rest {
        [parent, ..parents] ->
          write_ref(transactions_ref, [
            append_all(parent, transaction),
            ..parents
          ])
        [] -> {
          write_ref(transactions_ref, [])
          run_operations(transaction)
          flush(scheduler)
        }
      }
  }
}

fn run_operations(operations: List(fn() -> Nil)) -> Nil {
  case operations {
    [] -> Nil
    [operation, ..rest] -> {
      operation()
      run_operations(rest)
    }
  }
}

fn commit_cell(scheduler: Scheduler, cell: Cell(value), value: value) -> Nil {
  case same_value(read_cell(cell), value) {
    True -> Nil
    False -> {
      let Cell(
        value: value_ref,
        revision: revision_ref,
        subscribers: subscribers_ref,
        identity: identity,
        subscriber_ids: _,
      ) = cell
      write_ref(value_ref, value)
      write_ref(revision_ref, read_ref(revision_ref) + 1)
      let subscribers =
        schedule_subscribers(scheduler, identity, read_ref(subscribers_ref))
      write_ref(subscribers_ref, subscribers)
    }
  }
}

fn schedule_subscribers(
  scheduler: Scheduler,
  identity: Ref(Nil),
  subscribers: List(Subscriber),
) -> List(Subscriber) {
  case subscribers {
    [] -> []
    [Subscriber(id, callback) as subscriber, ..rest] ->
      case deref_weak(callback) {
        None -> schedule_subscribers(scheduler, identity, rest)
        Some(callback) -> {
          schedule(scheduler, ScheduledId(identity, id), callback)
          [subscriber, ..schedule_subscribers(scheduler, identity, rest)]
        }
      }
  }
}

fn schedule(
  scheduler: Scheduler,
  id: ScheduledId,
  callback: fn() -> Nil,
) -> Nil {
  let Scheduler(queue: queue_ref, queued: queued_ref, ..) = scheduler
  let queued = read_ref(queued_ref)
  case contains_scheduled_id(queued, id) {
    True -> Nil
    False -> {
      write_ref(queued_ref, append(queued, id))
      write_ref(queue_ref, append(read_ref(queue_ref), Scheduled(id, callback)))
    }
  }
}

fn flush(scheduler: Scheduler) -> Nil {
  let Scheduler(queue: queue_ref, queued: queued_ref, ..) = scheduler
  case read_ref(queue_ref) {
    [] -> Nil
    [Scheduled(id, callback), ..rest] -> {
      write_ref(queue_ref, rest)
      write_ref(queued_ref, remove_scheduled_id(read_ref(queued_ref), id))
      callback()
      flush(scheduler)
    }
  }
}

fn contains_scheduled_id(
  items: List(ScheduledId),
  target: ScheduledId,
) -> Bool {
  case items {
    [] -> False
    [item, ..rest] ->
      case item, target {
        ScheduledId(first_cell, first_subscriber),
          ScheduledId(second_cell, second_subscriber)
        ->
          same_value(first_cell, second_cell)
          && first_subscriber == second_subscriber
          || contains_scheduled_id(rest, target)
      }
  }
}

fn remove_scheduled_id(
  items: List(ScheduledId),
  target: ScheduledId,
) -> List(ScheduledId) {
  case items {
    [] -> []
    [item, ..rest] ->
      case item, target {
        ScheduledId(first_cell, first_subscriber),
          ScheduledId(second_cell, second_subscriber)
        ->
          case
            same_value(first_cell, second_cell)
            && first_subscriber == second_subscriber
          {
            True -> rest
            False -> [item, ..remove_scheduled_id(rest, target)]
          }
      }
  }
}

fn append(items: List(a), item: a) -> List(a) {
  case items {
    [] -> [item]
    [first, ..rest] -> [first, ..append(rest, item)]
  }
}

fn append_all(items: List(a), suffix: List(a)) -> List(a) {
  case suffix {
    [] -> items
    [first, ..rest] -> append_all(append(items, first), rest)
  }
}
