import dev_browser_test as browser
import gleam/io

const module_path = "/symbolize_lib_markup/lib_markup_browser_test_main.mjs"

pub fn main() {
  let root = browser.resolve_path("build/dev/javascript")
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
                    "lib-markup Chromium DOM FFI passed",
                  ])
                {
                  Error(reason) -> done(Error(reason))
                  Ok(Nil) -> {
                    io.println("lib-markup Chromium Gleam driver passed")
                    done(Ok(Nil))
                  }
                }
            }
          })
      }
    })
  }
}
