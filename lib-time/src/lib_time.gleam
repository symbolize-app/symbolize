pub type Context {
  Context(time: Time)
}

pub opaque type Time {
  System(SystemTime)
  Custom(now: fn() -> Float, set_timeout: fn(fn() -> Nil, Float) -> Nil)
}

pub type SystemTime

pub type Date

pub type AbortController

pub type AbortSignal

pub type Unit {
  Hours
  Minutes
  Seconds
}

pub type IntervalParts {
  IntervalParts(
    hours: Float,
    minutes: Float,
    seconds: Float,
    milliseconds: Float,
  )
}

pub fn interval_parts() -> IntervalParts {
  IntervalParts(hours: 0.0, minutes: 0.0, seconds: 0.0, milliseconds: 0.0)
}

@external(javascript, "./time_ffi.mjs", "new_system_time")
fn new_system_time() -> SystemTime

@external(javascript, "./time_ffi.mjs", "system_performance_now")
fn system_performance_now(time: SystemTime) -> Float

@external(javascript, "./time_ffi.mjs", "system_set_timeout")
fn system_set_timeout(
  time: SystemTime,
  callback: fn() -> Nil,
  milliseconds: Float,
) -> Nil

@external(javascript, "./time_ffi.mjs", "date_from_iso")
pub fn date_from_iso(iso: String) -> Date

@external(javascript, "./time_ffi.mjs", "date_get_time")
pub fn date_get_time(date: Date) -> Float

@external(javascript, "./time_ffi.mjs", "date_to_iso")
pub fn date_to_iso(date: Date) -> String

@external(javascript, "./time_ffi.mjs", "date_add")
fn date_add(date: Date, milliseconds: Float) -> Date

@external(javascript, "./time_ffi.mjs", "new_abort_controller")
pub fn new_abort_controller() -> AbortController

@external(javascript, "./time_ffi.mjs", "abort_controller_signal")
pub fn abort_controller_signal(controller: AbortController) -> AbortSignal

@external(javascript, "./time_ffi.mjs", "abort_controller_abort")
pub fn abort_controller_abort(controller: AbortController) -> Nil

@external(javascript, "./time_ffi.mjs", "signal_aborted")
pub fn signal_aborted(signal: AbortSignal) -> Bool

@external(javascript, "./time_ffi.mjs", "system_delay_or_abort")
fn system_delay_or_abort(
  time: SystemTime,
  signal: AbortSignal,
  milliseconds: Float,
  on_timer: fn() -> Nil,
  on_abort: fn() -> Nil,
) -> Nil

@external(javascript, "./time_ffi.mjs", "scheduled_delay_or_abort")
fn scheduled_delay_or_abort(
  set_timeout: fn(fn() -> Nil, Float) -> Nil,
  signal: AbortSignal,
  milliseconds: Float,
  on_timer: fn() -> Nil,
  on_abort: fn() -> Nil,
) -> Nil

pub fn time() -> Time {
  System(new_system_time())
}

pub fn custom(
  now: fn() -> Float,
  set_timeout: fn(fn() -> Nil, Float) -> Nil,
) -> Time {
  Custom(now: now, set_timeout: set_timeout)
}

pub fn new_context(time: Time) -> Context {
  Context(time: time)
}

pub fn performance_now(context: Context) -> Float {
  let Context(time) = context
  case time {
    System(system) -> system_performance_now(system)
    Custom(now: now, ..) -> now()
  }
}

pub fn set_timeout(
  context: Context,
  callback: fn() -> Nil,
  milliseconds: Float,
) -> Nil {
  let Context(time) = context
  case time {
    System(system) -> system_set_timeout(system, callback, milliseconds)
    Custom(set_timeout: set_timeout, ..) -> set_timeout(callback, milliseconds)
  }
}

pub fn delay(context: Context, milliseconds: Float, then: fn() -> Nil) -> Nil {
  set_timeout(context, then, milliseconds)
}

pub fn delay_or_abort(
  context: Context,
  signal: AbortSignal,
  milliseconds: Float,
  on_timer: fn() -> Nil,
  on_abort: fn() -> Nil,
) -> Nil {
  let Context(time) = context
  case time {
    System(system) ->
      system_delay_or_abort(system, signal, milliseconds, on_timer, on_abort)
    Custom(set_timeout: set_timeout, ..) ->
      scheduled_delay_or_abort(
        set_timeout,
        signal,
        milliseconds,
        on_timer,
        on_abort,
      )
  }
}

pub fn interval(parts: IntervalParts) -> Float {
  let IntervalParts(hours, minutes, seconds, milliseconds) = parts
  hours
  *. 60.0
  *. 60.0
  *. 1000.0
  +. minutes
  *. 60.0
  *. 1000.0
  +. seconds
  *. 1000.0
  +. milliseconds
}

pub fn convert(milliseconds: Float, unit: Unit) -> Float {
  let divisor = case unit {
    Hours -> 60.0 *. 60.0 *. 1000.0
    Minutes -> 60.0 *. 1000.0
    Seconds -> 1000.0
  }
  milliseconds /. divisor
}

pub fn subtract(to: Date, from: Date) -> Float {
  date_get_time(to) -. date_get_time(from)
}

pub fn add(initial: Date, milliseconds: Float) -> Date {
  date_add(initial, milliseconds)
}
