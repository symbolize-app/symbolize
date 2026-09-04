import gleam/option.{type Option, None, Some}
import lib_random
import lib_time

pub type Async(value, reason) =
  fn(fn(Result(value, reason)) -> Nil) -> Nil

pub type Context {
  Context(random: lib_random.Random, time: lib_time.Context)
}

pub type RetryConfig(reason) {
  RetryConfig(
    max_attempts: Int,
    min_delay_ms: Float,
    on_error: Option(fn(reason, Int, Float) -> Nil),
    signal: Option(lib_time.AbortSignal),
    window_ms: Float,
  )
}

pub fn new_context(
  random: lib_random.Random,
  time: lib_time.Context,
) -> Context {
  Context(random: random, time: time)
}

pub fn retry(
  context: Context,
  operation: Async(value, reason),
  config: RetryConfig(reason),
) -> Async(value, reason) {
  fn(done) {
    let start_ms = performance_now(context)
    retry_attempt(context, operation, config, done, start_ms, 0)
  }
}

fn retry_attempt(
  context: Context,
  operation: Async(value, reason),
  config: RetryConfig(reason),
  done: fn(Result(value, reason)) -> Nil,
  start_ms: Float,
  attempt: Int,
) -> Nil {
  operation(fn(result) {
    case result {
      Ok(value) -> done(Ok(value))
      Error(reason) ->
        case attempt == config.max_attempts - 1 {
          True -> done(Error(reason))
          False -> {
            let now_ms = performance_now(context)
            let #(random, jitter) = lib_random.number(context.random)
            let multiplier = 1.0 +. jitter *. power_of_two(attempt)
            let delay_ms = config.min_delay_ms *. multiplier
            let context = Context(random: random, time: context.time)
            case now_ms +. delay_ms >=. start_ms +. config.window_ms {
              True -> done(Error(reason))
              False -> {
                notify_error(config.on_error, reason, attempt, delay_ms)
                wait_for_retry(
                  context,
                  operation,
                  config,
                  done,
                  reason,
                  start_ms,
                  attempt,
                  delay_ms,
                )
              }
            }
          }
        }
    }
  })
}

fn wait_for_retry(
  context: Context,
  operation: Async(value, reason),
  config: RetryConfig(reason),
  done: fn(Result(value, reason)) -> Nil,
  reason: reason,
  start_ms: Float,
  attempt: Int,
  delay_ms: Float,
) -> Nil {
  let continue = fn() {
    retry_attempt(context, operation, config, done, start_ms, attempt + 1)
  }
  case config.signal {
    None -> lib_time.delay(context.time, delay_ms, continue)
    Some(signal) -> {
      case lib_time.signal_aborted(signal) {
        True ->
          lib_time.delay(context.time, delay_ms, fn() { done(Error(reason)) })
        False -> {
          lib_time.delay_or_abort(
            context.time,
            signal,
            delay_ms,
            fn() {
              case lib_time.signal_aborted(signal) {
                True -> done(Error(reason))
                False -> continue()
              }
            },
            fn() { done(Error(reason)) },
          )
        }
      }
    }
  }
}

fn notify_error(
  on_error: Option(fn(reason, Int, Float) -> Nil),
  reason: reason,
  attempt: Int,
  delay_ms: Float,
) -> Nil {
  case on_error {
    None -> Nil
    Some(on_error) -> on_error(reason, attempt, delay_ms)
  }
}

fn performance_now(context: Context) -> Float {
  lib_time.performance_now(context.time)
}

fn power_of_two(exponent: Int) -> Float {
  case exponent <= 0 {
    True -> 1.0
    False -> 2.0 *. power_of_two(exponent - 1)
  }
}
