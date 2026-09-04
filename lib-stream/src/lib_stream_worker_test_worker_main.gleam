import gleam/io
import lib_stream_source as source
import lib_stream_worker as worker
import lib_time

pub fn main() {
  let time = lib_time.new_context(lib_time.time())
  let server = worker.server(time)
  worker.serve_async(server, "echo", fn(output) {
    fn(data) {
      fn(done) {
        assert data == "a" || data == "b"
        source.send(time, output, "echo-" <> data, fn(result) {
          assert result == Ok(Nil)
          done(result)
        })
        case data == "b" {
          True ->
            source.close(output, fn(result) {
              assert result == Ok(Nil)
            })
          False -> Nil
        }
      }
    }
  })
  worker.serve_connect_async(server, "async-connect", fn(output) {
    fn(done) {
      lib_time.delay(time, 0.0, fn() {
        done(
          Ok(fn(data) {
            assert data == "request"
            source.send(time, output, "connected", fn(result) {
              assert result == Ok(Nil)
              source.close(output, fn(close_result) {
                assert close_result == Ok(Nil)
              })
            })
          }),
        )
      })
    }
  })
  io.println("lib-stream worker server ready")
}
