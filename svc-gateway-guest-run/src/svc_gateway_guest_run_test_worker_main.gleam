pub type ServiceWorker

@external(javascript, "./service_worker_ffi.mjs", "service_worker")
fn service_worker() -> ServiceWorker

@external(javascript, "./service_worker_ffi.mjs", "listen_install")
fn listen_install(worker: ServiceWorker, callback: fn() -> Nil) -> Nil

@external(javascript, "./service_worker_ffi.mjs", "skip_waiting")
fn skip_waiting(worker: ServiceWorker) -> Nil

pub fn main() {
  let worker = service_worker()
  listen_install(worker, fn() { skip_waiting(worker) })
}
