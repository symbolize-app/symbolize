import gleam/io
import lib_stream_source as source
import lib_stream_worker as worker
import lib_time

pub fn main(worker_url: String) {
  let time = lib_time.new_context(lib_time.time())
  let client = worker.new_worker(worker_url) |> worker.client
  let input =
    worker.connect_async(client, "echo", fn(data) {
      fn(done) {
        assert data == "echo-a" || data == "echo-b"
        case data == "echo-b" {
          True -> io.println("lib-stream Chromium Worker FFI passed")
          False -> Nil
        }
        done(Ok(Nil))
      }
    })
  source.send(time, input, "a", fn(result) {
    assert result == Ok(Nil)
    source.send(time, input, "b", fn(result) {
      assert result == Ok(Nil)
      source.close(input, fn(result) {
        assert result == Ok(Nil)
      })
    })
  })
}
