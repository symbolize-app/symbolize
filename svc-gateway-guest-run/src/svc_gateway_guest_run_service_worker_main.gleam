import svc_gateway_guest_run_service_worker_runtime as runtime

@external(javascript, "./service_worker_ffi.mjs", "injected_version")
fn injected_version() -> Int

@external(javascript, "./service_worker_ffi.mjs", "injected_manifest")
fn injected_manifest() -> List(#(String, String))

@external(javascript, "./service_worker_assets_ffi.mjs", "content_security_policy")
fn content_security_policy() -> String

@external(javascript, "./service_worker_assets_ffi.mjs", "font_css")
fn font_css() -> String

@external(javascript, "./service_worker_assets_ffi.mjs", "loader_css")
fn loader_css() -> String

@external(javascript, "./service_worker_assets_ffi.mjs", "reset_css")
fn reset_css() -> String

@external(javascript, "./service_worker_assets_ffi.mjs", "main_html")
fn main_html() -> String

pub fn main() {
  runtime.main(runtime.Config(
    version: injected_version(),
    manifest: injected_manifest(),
    content_security_policy: content_security_policy(),
    font_css: font_css(),
    loader_css: loader_css(),
    reset_css: reset_css(),
    main_html: main_html(),
  ))
}
