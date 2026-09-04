import gleam/io
import svc_gateway_guest_run_register as register
import svc_gateway_guest_run_reload as reload

pub fn main() {
  reload.listen_for_message()
  reload.listen_for_keyboard_shortcut()
  register.main()
  io.println("svc-gateway-guest-run reload listeners installed")
}
