import gleam/io
import svc_gateway_guest_run_reload as reload

pub type Navigator

pub type Registration

@external(javascript, "./register_ffi.mjs", "navigator")
fn navigator() -> Navigator

@external(javascript, "./register_ffi.mjs", "listen_for_controller_change")
fn listen_for_controller_change(
  navigator: Navigator,
  on_change: fn() -> Nil,
) -> Nil

@external(javascript, "./register_ffi.mjs", "register")
fn register(
  navigator: Navigator,
  path: String,
  scope: String,
  on_registered: fn(Registration) -> Nil,
  on_error: fn(String) -> Nil,
) -> Nil

@external(javascript, "./register_ffi.mjs", "listen_for_update")
fn listen_for_update(registration: Registration, on_update: fn() -> Nil) -> Nil

pub fn main() -> Nil {
  reload.listen_for_message()
  let navigator = navigator()
  listen_for_controller_change(navigator, fn() {
    io.println("controller change")
  })
  register(
    navigator,
    "/.code/svc-gateway-guest-run/serviceWorkerShell.js",
    "/",
    fn(registration) {
      listen_for_update(registration, fn() { io.println("update found") })
    },
    fn(reason) { panic as reason },
  )
}
