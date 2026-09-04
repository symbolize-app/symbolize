import dev_browser_test as browser
import gleam/io
import gleam/option.{type Option, None, Some}
import gleam/string
import lib_dataflow as dataflow

const stream_module = "/symbolize_lib_stream/lib_stream_browser_test_main.mjs"

const custom_module = "/symbolize_lib_stream/lib_stream_custom_http_test_main.mjs"

const worker_module = "/symbolize_lib_stream/lib_stream_worker_test_worker_main.mjs"

type State {
  State(
    context: dataflow.Context,
    attempts: dataflow.Mutation(Int),
    pending_response: dataflow.Mutation(Option(browser.HttpResponse)),
    saw_response_request: dataflow.Mutation(Bool),
    saw_request_stream: dataflow.Mutation(Bool),
  )
}

pub fn main() {
  let state =
    State(
      context: dataflow.dataflow(),
      attempts: dataflow.state(0),
      pending_response: dataflow.state(None),
      saw_response_request: dataflow.state(False),
      saw_request_stream: dataflow.state(False),
    )
  let root = browser.resolve_path("build/dev/javascript")
  browser.run_secure(
    fn(path, search, request, response) {
      handle(root, state, path, search, request, response)
    },
    fn(page, server, origin) { operation(page, server, origin, state) },
  )
}

fn handle(
  root: String,
  state: State,
  path: String,
  search: String,
  request: browser.HttpRequest,
  response: browser.HttpResponse,
) -> Nil {
  case path {
    "/stream" -> stream_route(state, search, request, response)
    "/worker-test-worker.mjs" ->
      browser.reply_text(
        response,
        200,
        "text/javascript",
        browser.module_script(worker_module, "main"),
      )
    "/favicon.ico" -> browser.reply_text(response, 404, "text/plain", "")
    _ -> browser.serve_static(root, path, response)
  }
}

fn stream_route(
  state: State,
  search: String,
  request: browser.HttpRequest,
  response: browser.HttpResponse,
) -> Nil {
  case string.contains(search, "response_stream_id") {
    True ->
      browser.read_request_body(request)(fn(body_result) {
        case body_result {
          Error(reason) ->
            browser.reply_text(response, 500, "text/plain", reason)
          Ok(body) -> {
            assert body == "ab"
            set(state.context, state.saw_request_stream, True)
            browser.end(response, "")
            case read(state.context, state.pending_response) {
              None -> panic as "response stream was not started"
              Some(pending) -> {
                browser.write(pending, "c")
                browser.end(pending, "d")
                set(state.context, state.pending_response, None)
              }
            }
          }
        }
      })
    False -> {
      let attempts = read(state.context, state.attempts) + 1
      set(state.context, state.attempts, attempts)
      case attempts {
        1 -> browser.reply_text(response, 503, "text/plain", "")
        _ -> {
          set(state.context, state.saw_response_request, True)
          browser.start_response(response, 200, [
            #("response-stream-id", "abcd"),
          ])
          browser.write(response, "c")
          set(state.context, state.pending_response, Some(response))
        }
      }
    }
  }
}

fn operation(
  page: browser.Page,
  server: browser.Server,
  origin: String,
  state: State,
) -> browser.Async(Nil) {
  fn(done) {
    let worker_url = origin <> "/worker-test-worker.mjs"
    browser.call_module(page, stream_module, [origin <> "/stream", worker_url])(
      fn(first_result) {
        case first_result {
          Error(reason) -> done(Error(reason))
          Ok(Nil) ->
            browser.call_module(page, custom_module, [])(fn(second_result) {
              case second_result {
                Error(reason) -> done(Error(reason))
                Ok(Nil) ->
                  browser.wait(5000)(fn(wait_result) {
                    case wait_result {
                      Error(reason) -> done(Error(reason))
                      Ok(Nil) -> done(check(page, server, state))
                    }
                  })
              }
            })
        }
      },
    )
  }
}

fn check(
  page: browser.Page,
  server: browser.Server,
  state: State,
) -> Result(Nil, String) {
  case browser.page_errors(page) {
    [] -> check_report(page, server, state)
    errors ->
      Error(
        "page errors: "
        <> browser.join_messages(errors)
        <> " console="
        <> browser.join_messages(browser.console_messages(page)),
      )
  }
}

fn check_report(
  page: browser.Page,
  server: browser.Server,
  state: State,
) -> Result(Nil, String) {
  case
    browser.report(page, server, [
      "lib-stream Chromium Web Streams FFI passed",
      "lib-stream Chromium Web WritableStream FFI passed",
      "lib-stream Chromium HTTP Fetch parity passed",
      "lib-stream Chromium Worker FFI passed",
      "lib-stream Chromium Worker async connect passed",
      "lib-stream custom Fetch context parity passed",
    ])
  {
    Error(reason) -> Error(reason)
    Ok(Nil) ->
      case
        read(state.context, state.saw_response_request)
        && read(state.context, state.saw_request_stream)
        && read(state.context, state.attempts) == 2
      {
        False -> Error("HTTP exchange incomplete")
        True -> check_failed_requests(browser.failed_requests(page))
      }
  }
}

fn check_failed_requests(values: List(String)) -> Result(Nil, String) {
  case values {
    [] -> {
      io.println("lib-stream Chromium Gleam driver passed")
      Ok(Nil)
    }
    [first, ..rest] ->
      case string.contains(first, "ERR_ABORTED") {
        True -> check_failed_requests(rest)
        False -> Error("unexpected browser request failure: " <> first)
      }
  }
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
