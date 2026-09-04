import gleam/io
import lib_time

pub fn main() {
  let from = lib_time.date_from_iso("2021-03-29T05:12:15.276Z")
  let added = lib_time.add(from, 12_055.0)
  let assert True = lib_time.date_to_iso(added) == "2021-03-29T05:12:27.331Z"
  let context = lib_time.new_context(lib_time.time())
  let now = lib_time.performance_now(context)
  let assert True = now >=. 0.0

  let controller = lib_time.new_abort_controller()
  let signal = lib_time.abort_controller_signal(controller)
  lib_time.set_timeout(
    context,
    fn() { lib_time.abort_controller_abort(controller) },
    0.0,
  )
  lib_time.delay_or_abort(
    context,
    signal,
    100.0,
    fn() { panic as "browser timer won abort race" },
    fn() { io.println("lib-time Chromium Web API FFI passed") },
  )
}
