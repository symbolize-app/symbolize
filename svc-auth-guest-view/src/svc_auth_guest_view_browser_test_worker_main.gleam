import lib_stream_source as source
import lib_stream_worker as worker
import lib_time

pub fn main() {
  let time = lib_time.new_context(lib_time.time())
  worker.serve_async(worker.server(time), "svc-auth-guest-read", fn(output) {
    fn(_data) { fn(done) { source.send(time, output, "pong", done) } }
  })
}
