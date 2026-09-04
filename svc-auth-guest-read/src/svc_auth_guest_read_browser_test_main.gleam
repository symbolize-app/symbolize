import gleam/io
import lib_stream_source as source
import lib_stream_worker as worker
import lib_time

pub fn main(worker_url: String) {
  let time = lib_time.new_context(lib_time.time())
  let client = worker.client(worker.new_worker(worker_url))
  let input =
    worker.connect(client, "svc-auth-guest-read", fn(data) {
      assert data == "pong"
      io.println("svc-auth-guest-read Worker FFI passed")
    })
  source.send(time, input, "ping", fn(result) {
    assert result == Ok(Nil)
  })
}
