import gleam/io
import lib_dataflow as dataflow
import lib_markup_dom as dom
import lib_markup_fragment as fragment
import lib_markup_html as html
import lib_markup_style as markup_style
import lib_stream_source as source
import lib_stream_worker as worker
import lib_time
import svc_auth_guest_view_custom as custom
import svc_auth_guest_view_styles as styles

pub type Context {
  Context(
    document: dom.Document,
    runtime: dataflow.Context,
    host: markup_style.Host,
    worker_client: worker.Client,
    time: lib_time.Context,
  )
}

pub fn main(context: Context) -> #(markup_style.Host, dom.Mounted) {
  let Context(document, runtime, host, worker_client, time) = context
  let document_element = dom.document_element(document)
  let body = dom.body(document)
  let client_source =
    worker.connect(worker_client, "svc-auth-guest-read", fn(data) {
      io.println("client data " <> data)
    })
  source.send(time, client_source, "ping", fn(_result) { Nil })
  let #(host, view) = build(document, host, dataflow.literal("st ffb hello"))
  let root =
    html.portal(
      body,
      html.DivAttrs(..html.div_attrs(), content: fragment.FragmentInput(view)),
    )
  let #(host, mounted) = markup_style.mount_reactive(runtime, host, body, root)
  dom.remove_class(document_element, "loading")
  #(host, mounted)
}

pub fn build(
  document: dom.Document,
  host: markup_style.Host,
  title: dataflow.NodeOpt(String),
) -> #(markup_style.Host, fragment.Fragment) {
  let #(host, fill_var) = markup_style.variable(host)
  let #(host, grid_color) = markup_style.variable(host)
  let html_style = styles.html_style(document)
  let #(host, body_style) =
    styles.body_style_with_variable(host, document, grid_color)
  let #(host, extra) = markup_style.container(host)
  let custom = custom.custom(dom.head(document), fill_var, extra, title)
  let value =
    fragment.range([
      fragment.FragmentInput(html_style),
      fragment.FragmentInput(body_style),
      fragment.FragmentInput(custom),
    ])
  #(host, value)
}
