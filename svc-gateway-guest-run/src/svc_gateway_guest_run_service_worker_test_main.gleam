import gleam/io
import svc_gateway_guest_run_service_worker as service_worker

pub fn main() {
  assert service_worker.route("/.code/.id/abc.js")
    == service_worker.ContentById("abc.js")
  assert service_worker.route("/.code/main.mjs")
    == service_worker.ContentByPath("main.mjs")
  assert service_worker.route("/.code/.id/")
    == service_worker.ContentByPath(".id/")
  assert service_worker.route("/") == service_worker.MainHtml

  assert service_worker.content_type("main.html") == Ok("text/html")
  assert service_worker.content_type("worker.js") == Ok("text/javascript")
  assert service_worker.content_type("worker.mjs") == Ok("text/javascript")
  assert service_worker.content_type("font.woff2") == Ok("font/woff2")
  assert service_worker.content_type("font.ttf")
    == Error("Unknown content type for font.ttf")

  assert service_worker.headers("  policy  ", "main.html")
    == Ok([
      #("content-security-policy", "  policy"),
      #("content-type", "text/html"),
    ])
  io.println("svc-gateway-guest-run service-worker rules passed")
}
