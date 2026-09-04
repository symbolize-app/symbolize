import gleam/io
import lib_dataflow as dataflow
import lib_markup_dom as dom
import lib_markup_style as markup_style
import lib_stream_worker as worker
import lib_time
import svc_auth_guest_view_main as view
import svc_gateway_guest_run_reload as reload

pub fn main() {
  reload.listen_for_message()
  reload.listen_for_keyboard_shortcut()
  let document = dom.document()
  let client =
    worker.client(worker.new_worker(
      "/.code/svc-gateway-guest-run/dedicatedWorker.mjs",
    ))
  let context =
    view.Context(
      document: document,
      runtime: dataflow.dataflow(),
      host: markup_style.new(document),
      worker_client: client,
      time: lib_time.new_context(lib_time.time()),
    )
  let #(_host, _mounted) = view.main(context)
  io.println("svc-gateway-guest-run main mounted")
}
