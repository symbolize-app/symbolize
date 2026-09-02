import gleam/io
import lib_dataflow as dataflow
import lib_markup_dom as dom
import lib_markup_style as markup_style
import lib_stream_worker as worker
import lib_time
import svc_auth_guest_view_main as view

pub fn main(worker_url: String) -> dom.Mounted {
  let document = dom.document()
  let runtime = dataflow.dataflow()
  let host = markup_style.new(document)
  let client = worker.client(worker.new_worker(worker_url))
  let context =
    view.Context(
      document: document,
      runtime: runtime,
      host: host,
      worker_client: client,
      time: lib_time.new_context(lib_time.time()),
    )
  let #(_host, mounted) = view.main(context)
  io.println("svc-auth-guest-view custom content mounted")
  mounted
}
