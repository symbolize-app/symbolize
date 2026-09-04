import dev_browser_test as browser
import gleam/option.{type Option, None, Some}
import gleam/string
import lib_dataflow as dataflow
import lib_stream_http_test_main as http_test

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
  browser.server(fn(path, search, request, response) {
    handle(state, path, search, request, response)
  })(fn(server_result) {
    case server_result {
      Error(reason) -> fail(reason)
      Ok(#(server, origin)) -> {
        http_test.main(origin <> "/stream")
        wait_for_exchange(state, 100, fn(result) {
          browser.close_server(server)(fn(close_result) {
            case result, close_result {
              Error(reason), _ -> fail(reason)
              Ok(Nil), Error(reason) -> fail(reason)
              Ok(Nil), Ok(Nil) -> Nil
            }
          })
        })
      }
    }
  })
}

fn handle(
  state: State,
  path: String,
  search: String,
  request: browser.HttpRequest,
  response: browser.HttpResponse,
) -> Nil {
  case path == "/stream" {
    False -> browser.reply_text(response, 404, "text/plain", "")
    True ->
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
}

fn wait_for_exchange(
  state: State,
  remaining: Int,
  done: fn(Result(Nil, String)) -> Nil,
) -> Nil {
  case complete(state) {
    True -> done(Ok(Nil))
    False ->
      case remaining {
        0 -> done(check(state))
        _ ->
          browser.wait(50)(fn(wait_result) {
            case wait_result {
              Error(reason) -> done(Error(reason))
              Ok(Nil) -> wait_for_exchange(state, remaining - 1, done)
            }
          })
      }
  }
}

fn check(state: State) -> Result(Nil, String) {
  case complete(state) {
    True -> Ok(Nil)
    False -> Error("HTTP exchange incomplete")
  }
}

fn complete(state: State) -> Bool {
  read(state.context, state.saw_response_request)
  && read(state.context, state.saw_request_stream)
  && read(state.context, state.attempts) == 2
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

fn fail(reason: String) -> Nil {
  panic as reason
}
