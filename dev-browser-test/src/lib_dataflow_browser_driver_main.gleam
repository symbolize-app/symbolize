import dev_browser_test as browser
import gleam/io

const module_path = "/symbolize_lib_dataflow/lib_dataflow_test_main.mjs"

pub fn main() {
  let root = browser.resolve_path("../lib-dataflow/build/dev/javascript")
  browser.run(
    fn(path, _search, _request, response) {
      browser.serve_static(root, path, response)
    },
    operation,
  )
}

fn operation(
  page: browser.Page,
  server: browser.Server,
  _origin: String,
) -> browser.Async(Nil) {
  fn(done) {
    browser.call_module(page, module_path, [])(fn(result) {
      case result {
        Error(reason) -> done(Error(reason))
        Ok(Nil) ->
          browser.wait(100)(fn(wait_result) {
            case wait_result {
              Error(reason) -> done(Error(reason))
              Ok(Nil) ->
                case
                  browser.report(page, server, [
                    "lib-dataflow explicit Gleam parity slice passed",
                    "lib-dataflow async callback parity passed",
                    "lib-dataflow async error parity passed",
                    "lib-dataflow async rejection propagation passed",
                    "lib-dataflow async multi-value parity passed",
                    "lib-dataflow async multi-input cache parity passed",
                  ])
                {
                  Error(reason) -> done(Error(reason))
                  Ok(Nil) -> {
                    io.println("lib-dataflow Chromium Gleam driver passed")
                    done(Ok(Nil))
                  }
                }
            }
          })
      }
    })
  }
}
