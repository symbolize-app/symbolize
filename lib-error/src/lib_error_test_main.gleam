import gleam/io
import gleam/option
import lib_dataflow as dataflow
import lib_error
import lib_random
import lib_time

type Timer {
  Timer(at: Float, callback: fn() -> Nil)
}

type TestState {
  TestState(
    context: dataflow.Context,
    now: dataflow.Mutation(Float),
    sequence_attempts: dataflow.Mutation(Int),
    timers: dataflow.Mutation(List(Timer)),
    callback_count: dataflow.Mutation(Int),
  )
}

fn new_test_state() -> TestState {
  let context = dataflow.dataflow()
  TestState(
    context: context,
    now: dataflow.state(0.0),
    sequence_attempts: dataflow.state(0),
    timers: dataflow.state([]),
    callback_count: dataflow.state(0),
  )
}

fn reset_test_state(state: TestState) -> Nil {
  let TestState(context, now, sequence_attempts, timers, callback_count) = state
  write_state(context, now, 0.0)
  write_state(context, sequence_attempts, 0)
  write_state(context, timers, [])
  write_state(context, callback_count, 0)
}

fn clock_now(state: TestState) -> Float {
  let TestState(_, now, _, _, _) = state
  read_state(now)
}

fn clock_set_timeout(
  state: TestState,
  callback: fn() -> Nil,
  milliseconds: Float,
) -> Nil {
  let TestState(context, _, _, timers, _) = state
  let timer = Timer(clock_now(state) +. milliseconds, callback)
  write_state(context, timers, append_timer(read_state(timers), timer))
}

fn clock_tick(state: TestState, milliseconds: Float) -> Nil {
  let target = clock_now(state) +. milliseconds
  run_due_timers(state, target)
  let TestState(context, now, _, _, _) = state
  write_state(context, now, target)
}

fn run_due_timers(state: TestState, target: Float) -> Nil {
  let TestState(context, now, _, timers, _) = state
  case next_timer(read_state(timers), target) {
    option.None -> Nil
    option.Some(index) -> {
      let assert option.Some(#(Timer(at, callback), remaining)) =
        remove_timer(read_state(timers), index)
      write_state(context, timers, remaining)
      write_state(context, now, at)
      callback()
      run_due_timers(state, target)
    }
  }
}

fn next_timer(timers: List(Timer), target: Float) -> option.Option(Int) {
  next_timer_at(timers, target, 0, option.None)
}

fn next_timer_at(
  timers: List(Timer),
  target: Float,
  index: Int,
  best: option.Option(#(Int, Float)),
) -> option.Option(Int) {
  case timers {
    [] -> option.map(best, fn(item) { item.0 })
    [Timer(at, _), ..rest] -> {
      let best = case at <=. target {
        False -> best
        True ->
          case best {
            option.None -> option.Some(#(index, at))
            option.Some(#(_, best_at)) ->
              case at <. best_at {
                True -> option.Some(#(index, at))
                False -> best
              }
          }
      }
      next_timer_at(rest, target, index + 1, best)
    }
  }
}

fn remove_timer(
  timers: List(Timer),
  index: Int,
) -> option.Option(#(Timer, List(Timer))) {
  case timers {
    [] -> option.None
    [timer, ..rest] ->
      case index == 0 {
        True -> option.Some(#(timer, rest))
        False ->
          case remove_timer(rest, index - 1) {
            option.None -> option.None
            option.Some(#(removed, remaining)) ->
              option.Some(#(removed, [timer, ..remaining]))
          }
      }
  }
}

fn append_timer(timers: List(Timer), timer: Timer) -> List(Timer) {
  case timers {
    [] -> [timer]
    [head, ..tail] -> [head, ..append_timer(tail, timer)]
  }
}

fn mark_callback(state: TestState) -> Nil {
  let TestState(context, _, _, _, count) = state
  write_state(context, count, read_state(count) + 1)
}

fn callback_count(state: TestState) -> Int {
  let TestState(_, _, _, _, count) = state
  read_state(count)
}

fn fail_twice_then_pass(
  state: TestState,
  done: fn(Result(String, String)) -> Nil,
) -> Nil {
  let TestState(context, _, attempts, _, _) = state
  let current = read_state(attempts) + 1
  write_state(context, attempts, current)
  case current <= 2 {
    True -> done(Error("TEST"))
    False -> done(Ok("PASS"))
  }
}

fn read_state(state: dataflow.Mutation(value)) -> value {
  dataflow.value(dataflow.to_computation(dataflow.mutation(state)))
}

fn write_state(
  context: dataflow.Context,
  state: dataflow.Mutation(value),
  value: value,
) -> Nil {
  let assert Ok(Nil) =
    dataflow.txn(context, fn() { dataflow.set(context, state, value) })
  Nil
}

pub fn run(done: fn() -> Nil) {
  let state = new_test_state()
  reset_test_state(state)
  let context = test_context(state, 0.7)

  assert_result_ok(lib_error.retry(
    context,
    fn(done) { done(Ok(42)) },
    lib_error.RetryConfig(
      max_attempts: 1,
      min_delay_ms: 10.0,
      on_error: option.None,
      signal: option.None,
      window_ms: 100.0,
    ),
  ))

  let count_limited =
    lib_error.retry(
      context,
      fn(done) { done(Error("TEST")) },
      lib_error.RetryConfig(
        max_attempts: 2,
        min_delay_ms: 10.0,
        on_error: option.Some(fn(reason, attempt, delay_ms) {
          assert reason == "TEST"
          assert attempt == 0
          assert delay_ms == 17.0
        }),
        signal: option.None,
        window_ms: 100.0,
      ),
    )
  let _ =
    count_limited(fn(result) {
      mark_callback(state)
      let assert Error("TEST") = result
      Nil
    })
  assert callback_count(state) == 0
  clock_tick(state, 16.0)
  assert callback_count(state) == 0
  clock_tick(state, 1.0)
  assert callback_count(state) == 1

  reset_test_state(state)
  let context = test_context(state, 0.8)
  let three_attempts =
    lib_error.retry(
      context,
      fn(done) { done(Error("TEST")) },
      lib_error.RetryConfig(
        max_attempts: 3,
        min_delay_ms: 10.0,
        on_error: option.Some(fn(reason, attempt, delay_ms) {
          assert reason == "TEST"
          case attempt {
            0 -> {
              assert delay_ms == 18.0
            }
            1 -> {
              assert delay_ms == 26.0
            }
            _ -> panic as "unexpected retry attempt"
          }
        }),
        signal: option.None,
        window_ms: 10_000.0,
      ),
    )
  let _ =
    three_attempts(fn(result) {
      mark_callback(state)
      let assert Error("TEST") = result
      Nil
    })
  assert callback_count(state) == 0
  clock_tick(state, 18.0)
  assert callback_count(state) == 0
  clock_tick(state, 25.0)
  assert callback_count(state) == 0
  clock_tick(state, 1.0)
  assert callback_count(state) == 1

  reset_test_state(state)
  let context = test_context(state, 0.0)
  let window_limited =
    lib_error.retry(
      context,
      fn(done) {
        clock_set_timeout(state, fn() { done(Error("WINDOW")) }, 9000.0)
      },
      lib_error.RetryConfig(
        max_attempts: 10,
        min_delay_ms: 1000.0,
        on_error: option.None,
        signal: option.None,
        window_ms: 10_000.0,
      ),
    )
  let _ =
    window_limited(fn(result) {
      mark_callback(state)
      let assert Error("WINDOW") = result
      Nil
    })
  assert callback_count(state) == 0
  clock_tick(state, 8999.0)
  assert callback_count(state) == 0
  clock_tick(state, 1.0)
  assert callback_count(state) == 1

  reset_test_state(state)
  let context = test_context(state, 0.6)
  let window_three_attempts =
    lib_error.retry(
      context,
      fn(done) { done(Error("WINDOW")) },
      lib_error.RetryConfig(
        max_attempts: 10,
        min_delay_ms: 1000.0,
        on_error: option.Some(fn(reason, attempt, delay_ms) {
          assert reason == "WINDOW"
          case attempt {
            0 -> {
              assert delay_ms == 1600.0
            }
            1 -> {
              assert delay_ms == 2200.0
            }
            _ -> panic as "unexpected window attempt"
          }
        }),
        signal: option.None,
        window_ms: 3801.0,
      ),
    )
  let _ =
    window_three_attempts(fn(result) {
      mark_callback(state)
      let assert Error("WINDOW") = result
      Nil
    })
  assert callback_count(state) == 0
  clock_tick(state, 1600.0)
  assert callback_count(state) == 0
  clock_tick(state, 2199.0)
  assert callback_count(state) == 0
  clock_tick(state, 1.0)
  assert callback_count(state) == 1

  reset_test_state(state)
  let context = test_context(state, 0.8)
  let pass_after_retries =
    lib_error.retry(
      context,
      fn(done) { fail_twice_then_pass(state, done) },
      lib_error.RetryConfig(
        max_attempts: 3,
        min_delay_ms: 10.0,
        on_error: option.Some(fn(reason, attempt, delay_ms) {
          assert reason == "TEST"
          case attempt {
            0 -> {
              assert delay_ms == 18.0
            }
            1 -> {
              assert delay_ms == 26.0
            }
            _ -> panic as "unexpected success attempt"
          }
        }),
        signal: option.None,
        window_ms: 10_000.0,
      ),
    )
  let _ =
    pass_after_retries(fn(result) {
      mark_callback(state)
      let assert Ok("PASS") = result
      Nil
    })
  assert callback_count(state) == 0
  clock_tick(state, 18.0)
  assert callback_count(state) == 0
  clock_tick(state, 26.0)
  assert callback_count(state) == 1

  reset_test_state(state)
  let context = test_context(state, 0.0)
  assert_result_error(
    lib_error.retry(
      context,
      fn(done) { done(Error("WINDOW")) },
      lib_error.RetryConfig(
        max_attempts: 10,
        min_delay_ms: 10.0,
        on_error: option.None,
        signal: option.None,
        window_ms: 10.0,
      ),
    ),
    "WINDOW",
  )

  let controller = lib_time.new_abort_controller()
  let signal = lib_time.abort_controller_signal(controller)
  let aborting_time =
    lib_time.new_context(
      lib_time.custom(fn() { clock_now(state) }, fn(callback, milliseconds) {
        clock_set_timeout(state, callback, milliseconds)
      }),
    )
  let aborting =
    lib_error.new_context(
      lib_random.custom(fn(_bits) { <<>> }, fn() { 0.5 }),
      aborting_time,
    )
  let aborting_retry =
    lib_error.retry(
      aborting,
      fn(done) { done(Error("ABORT")) },
      lib_error.RetryConfig(
        max_attempts: 10,
        min_delay_ms: 100.0,
        on_error: option.None,
        signal: option.Some(signal),
        window_ms: 1000.0,
      ),
    )
  let _ =
    aborting_retry(fn(result) {
      mark_callback(state)
      let assert Error("ABORT") = result
      io.println("lib-error abort retry passed")
      io.println("lib-error Gleam parity tests passed")
      done()
    })
  assert callback_count(state) == 0
  clock_tick(state, 100.0)
  assert callback_count(state) == 0
  lib_time.abort_controller_abort(controller)
  assert callback_count(state) == 1
}

pub fn main() {
  run(fn() { Nil })
}

fn test_context(state: TestState, jitter: Float) -> lib_error.Context {
  let clock =
    lib_time.new_context(
      lib_time.custom(fn() { clock_now(state) }, fn(callback, milliseconds) {
        clock_set_timeout(state, callback, milliseconds)
      }),
    )
  let random = lib_random.custom(fn(_bits) { <<>> }, fn() { jitter })
  lib_error.new_context(random, clock)
}

fn assert_result_ok(operation: lib_error.Async(Int, String)) {
  let _ =
    operation(fn(result) {
      let assert Ok(value) = result
      let assert True = value == 42
      Nil
    })
}

fn assert_result_error(
  operation: lib_error.Async(value, String),
  expected: String,
) {
  let _ =
    operation(fn(result) {
      let assert Error(value) = result
      let assert True = value == expected
      Nil
    })
}
