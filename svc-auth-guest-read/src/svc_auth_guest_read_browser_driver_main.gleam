import dev_browser_test as browser
import gleam/io
import gleam/option.{type Option, None, Some}
import gleam/string
import lib_dataflow as dataflow

const module_path = "/symbolize_svc_auth_guest_read/svc_auth_guest_read_browser_test_main.mjs"

const worker_module = "/symbolize_svc_auth_guest_read/svc_auth_guest_read_main.mjs"

type State {
  State(
    context: dataflow.Context,
    pending_response: dataflow.Mutation(Option(browser.HttpResponse)),
  )
}

pub fn main() {
  let state = State(dataflow.dataflow(), dataflow.state(None))
  let root = browser.resolve_path("build/dev/javascript")
  browser.run(
    fn(path, search, _request, response) {
      handle(root, state, path, search, response)
    },
    fn(page, server, origin) { operation(page, server, origin, state) },
  )
}

fn handle(
  root: String,
  state: State,
  path: String,
  search: String,
  response: browser.HttpResponse,
) -> Nil {
  case path {
    "/.stream" -> {
      case string.contains(search, "response_stream_id") {
        True -> {
          browser.end(response, "")
          case read(state.context, state.pending_response) {
            None -> Nil
            Some(pending) -> {
              browser.end(pending, "stream-ready")
              set(state.context, state.pending_response, None)
            }
          }
        }
        False -> {
          browser.start_response(response, 200, [
            #("response-stream-id", "abcd"),
          ])
          browser.write(response, "stream-ready")
          set(state.context, state.pending_response, Some(response))
        }
      }
    }
    "/svc-auth-guest-read-worker.mjs" ->
      browser.reply_text(
        response,
        200,
        "text/javascript",
        browser.module_script(worker_module, "main"),
      )
    _ -> browser.serve_static(root, path, response)
  }
}

fn operation(
  page: browser.Page,
  server: browser.Server,
  origin: String,
  _state: State,
) -> browser.Async(Nil) {
  browser.bind(
    browser.call_module(page, module_path, [
      origin <> "/svc-auth-guest-read-worker.mjs",
    ]),
    fn(_ignored) {
      browser.bind(browser.wait(5000), fn(_ignored) {
        fn(done) {
          case
            browser.report(page, server, [
              "svc-auth-guest-read Worker FFI passed",
            ])
          {
            Error(reason) -> done(Error(reason))
            Ok(Nil) -> {
              io.println("svc-auth-guest-read Chromium Gleam driver passed")
              done(Ok(Nil))
            }
          }
        }
      })
    },
  )
}

fn read(
  _context: dataflow.Context,
  mutation: dataflow.Mutation(value),
) -> value {
  dataflow.value(dataflow.to_computation(dataflow.mutation(mutation)))
}

fn set(
  context: dataflow.Context,
  mutation: dataflow.Mutation(value),
  value: value,
) -> Nil {
  let assert Ok(Nil) =
    dataflow.txn(context, fn() { dataflow.set(context, mutation, value) })
  Nil
}
