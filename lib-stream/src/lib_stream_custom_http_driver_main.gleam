import dev_browser_test as browser
import gleam/io
import lib_stream_custom_http_test_main as custom_test

pub fn main() {
  custom_test.main()
  browser.wait(250)(fn(result) {
    case result {
      Error(reason) -> panic as reason
      Ok(Nil) -> io.println("lib-stream custom Fetch context parity passed")
    }
  })
}
