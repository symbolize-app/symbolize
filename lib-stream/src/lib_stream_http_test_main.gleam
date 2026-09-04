import gleam/io
import lib_stream_http as http
import lib_stream_source as source
import lib_time

pub fn main(url: String) {
  let client =
    http.client(http.context(), url, fn(data) {
      assert data == "c"
      io.println("lib-stream HTTP Fetch parity passed")
    })
  let input = http.source(client)
  let time = lib_time.new_context(lib_time.time())
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
