import gleam/io
import gleam/option
import lib_error
import lib_random
import lib_time

pub fn main() {
  let time = lib_time.new_context(lib_time.time())
  let controller = lib_time.new_abort_controller()
  let signal = lib_time.abort_controller_signal(controller)
  let context = lib_error.new_context(lib_random.random(), time)

  lib_time.set_timeout(
    time,
    fn() { lib_time.abort_controller_abort(controller) },
    0.0,
  )

  let _ =
    lib_error.retry(
      context,
      fn(done) { done(Error("ABORT")) },
      lib_error.RetryConfig(
        max_attempts: 10,
        min_delay_ms: 100.0,
        on_error: option.None,
        signal: option.Some(signal),
        window_ms: 1000.0,
      ),
    )(fn(result) {
      let assert Error("ABORT") = result
      io.println("lib-error Chromium AbortController retry passed")
    })
}
