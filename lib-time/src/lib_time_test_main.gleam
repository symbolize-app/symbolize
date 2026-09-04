import gleam/io
import lib_time

pub fn run(done: fn() -> Nil) {
  assert lib_time.interval(lib_time.interval_parts()) == 0.0
  assert lib_time.interval(
      lib_time.IntervalParts(..lib_time.interval_parts(), seconds: 9.0),
    )
    == 9000.0
  let parts =
    lib_time.IntervalParts(
      hours: 2.0,
      minutes: 2.0,
      seconds: 2.0,
      milliseconds: 2.0,
    )
  assert lib_time.interval(parts) == 7_322_002.0
  assert lib_time.convert(7_200_000.0, lib_time.Hours) == 2.0
  assert lib_time.convert(7_200_000.0, lib_time.Minutes) == 120.0
  assert lib_time.convert(7_200_000.0, lib_time.Seconds) == 7200.0

  let from = lib_time.date_from_iso("2021-03-29T05:12:15.276Z")
  let to = lib_time.date_from_iso("2021-03-29T05:12:27.331Z")
  assert lib_time.subtract(to, from) == 12_055.0
  assert lib_time.date_to_iso(lib_time.add(from, 12_055.0))
    == "2021-03-29T05:12:27.331Z"

  let custom =
    lib_time.custom(fn() { 123.5 }, fn(callback, milliseconds) {
      assert milliseconds == 20.0
      callback()
    })
  let context = lib_time.new_context(custom)
  assert lib_time.performance_now(context) == 123.5
  lib_time.delay(context, 20.0, fn() { Nil })

  let system = lib_time.new_context(lib_time.time())
  lib_time.delay(system, 0.0, fn() {
    io.println("lib-time system timer callback passed")
    let now = lib_time.performance_now(system)
    assert now >=. 0.0
    let delay_start = now
    lib_time.delay(system, 20.0, fn() {
      assert lib_time.performance_now(system) -. delay_start >=. 10.0
      io.println("lib-time delay parity passed")

      let controller = lib_time.new_abort_controller()
      let signal = lib_time.abort_controller_signal(controller)
      lib_time.set_timeout(
        system,
        fn() { lib_time.abort_controller_abort(controller) },
        0.0,
      )
      lib_time.delay_or_abort(
        system,
        signal,
        100.0,
        fn() { panic as "timer won an aborted delay" },
        fn() {
          io.println("lib-time abort race passed")

          let custom_controller = lib_time.new_abort_controller()
          let custom_signal =
            lib_time.abort_controller_signal(custom_controller)
          let custom_time =
            lib_time.custom(fn() { 0.0 }, fn(callback, _milliseconds) {
              lib_time.abort_controller_abort(custom_controller)
              callback()
            })
          let custom_context = lib_time.new_context(custom_time)
          lib_time.delay_or_abort(
            custom_context,
            custom_signal,
            100.0,
            fn() { panic as "synchronous custom timer won an abort race" },
            fn() {
              io.println("lib-time custom abort race passed")
              io.println("lib-time Gleam parity tests passed")
              done()
            },
          )
        },
      )
    })
  })
}

pub fn main() {
  run(fn() { Nil })
}
