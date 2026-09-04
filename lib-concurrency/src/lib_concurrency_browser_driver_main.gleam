import dev_browser_test as browser
import gleam/io
import gleam/option.{type Option, None, Some}

const module_path = "/symbolize_lib_concurrency/lib_concurrency_test_main.mjs"

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
              Ok(Nil) -> done(check(page, server))
            }
          })
      }
    })
  }
}

fn check(page: browser.Page, server: browser.Server) -> Result(Nil, String) {
  case
    browser.report(page, server, [
      "lib-concurrency first waiter passed",
      "lib-concurrency second waiter passed",
      "lib-concurrency Promise semaphore parity passed",
    ])
  {
    Error(reason) -> Error(reason)
    Ok(Nil) -> {
      let messages = browser.console_messages(page)
      let first = index_of(messages, "lib-concurrency first waiter passed", 0)
      let second = index_of(messages, "lib-concurrency second waiter passed", 0)
      let parity =
        count(messages, "lib-concurrency Promise semaphore parity passed")
      case first, second, parity {
        Some(first), Some(second), 1 if first < second -> {
          io.println("lib-concurrency Chromium Promise FFI passed")
          Ok(Nil)
        }
        _, _, _ -> Error("lib-concurrency browser ordering mismatch")
      }
    }
  }
}

fn index_of(values: List(String), expected: String, index: Int) -> Option(Int) {
  case values {
    [] -> None
    [first, ..rest] ->
      case first == expected {
        True -> Some(index)
        False -> index_of(rest, expected, index + 1)
      }
  }
}

fn count(values: List(String), expected: String) -> Int {
  case values {
    [] -> 0
    [first, ..rest] ->
      case first == expected {
        True -> 1 + count(rest, expected)
        False -> count(rest, expected)
      }
  }
}
