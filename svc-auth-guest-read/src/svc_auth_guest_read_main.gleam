import gleam/io
import lib_stream_http as http
import lib_stream_source as source
import lib_stream_worker as worker
import lib_time

pub fn main() {
  let http_context = http.context()
  let time = http.time(http_context)

  worker.serve_async(
    worker.server(time),
    "svc-auth-guest-read",
    fn(server_source) {
      fn(data) {
        fn(done) {
          io.println("server data " <> data)
          source.send(time, server_source, "pong", fn(result) { done(result) })
        }
      }
    },
  )

  let client =
    http.client(http_context, http.browser_url("/.stream"), fn(data) {
      io.println("stream response " <> data)
    })
  let input = http.source(client)
  send_or_panic(time, input, "hello", fn() { repeat_stream(time, input) })
}

fn repeat_stream(time: lib_time.Context, input: source.Source(String)) -> Nil {
  lib_time.delay(time, 10_000.0, fn() {
    send_or_panic(time, input, "world", fn() { repeat_stream(time, input) })
  })
}

fn send_or_panic(
  time: lib_time.Context,
  input: source.Source(String),
  value: String,
  done: fn() -> Nil,
) -> Nil {
  source.send(time, input, value, fn(result) {
    case result {
      Ok(Nil) -> done()
      Error(_) -> panic as "failed to send HTTP request stream"
    }
  })
}
