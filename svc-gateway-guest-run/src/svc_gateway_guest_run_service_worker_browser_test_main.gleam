import svc_gateway_guest_run_service_worker_runtime as runtime

pub fn main() {
  runtime.main(runtime.Config(
    version: 1,
    manifest: [#("app.js", "app.js")],
    content_security_policy: "test-policy  \n",
    font_css: "FONT",
    loader_css: "LOADER",
    reset_css: "RESET",
    main_html: "<!doctype html><html><head><title>Gleam service worker</title></head><body>@import url('font.css');@import url('loader.css');@import url('reset.css');</body></html>",
  ))
}
