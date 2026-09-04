import dev_browser_test as browser
import gleam/io
import gleam/bit_array
import gleam/option.{type Option, None, Some}
import gleam/string
import lib_dataflow as dataflow

const reload_module = "/symbolize_svc_gateway_guest_run/svc_gateway_guest_run_reload_browser_test_main.mjs"

const main_module = "/symbolize_svc_gateway_guest_run/svc_gateway_guest_run_main.mjs"

const dedicated_worker_module = "/symbolize_svc_gateway_guest_run/svc_gateway_guest_run_dedicated_worker_main.mjs"

const test_worker_module = "/symbolize_svc_gateway_guest_run/svc_gateway_guest_run_test_worker_main.mjs"

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
    "/test-worker.js" ->
      browser.reply_text(
        response,
        200,
        "text/javascript",
        browser.module_script(test_worker_module, "main"),
      )
    "/.code/svc-gateway-guest-run/dedicatedWorker.mjs" ->
      browser.reply_text(
        response,
        200,
        "text/javascript",
        browser.module_script(dedicated_worker_module, "main"),
      )
    "/.code/svc-gateway-guest-run/serviceWorkerShell.js" ->
      browser.reply(
        response,
        200,
        [
          #("content-type", "text/javascript"),
          #("service-worker-allowed", "/"),
        ],
        bit_array.from_string(
          "self.addEventListener('install', () => self.skipWaiting())",
        ),
      )
    "/.stream" -> stream_route(state, search, response)
    _ -> browser.serve_static(root, path, response)
  }
}

fn stream_route(
  state: State,
  search: String,
  response: browser.HttpResponse,
) -> Nil {
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
      browser.start_response(response, 200, [#("response-stream-id", "abcd")])
      browser.write(response, "stream-ready")
      set(state.context, state.pending_response, Some(response))
    }
  }
}

fn operation(
  page: browser.Page,
  server: browser.Server,
  _origin: String,
  _state: State,
) -> browser.Async(Nil) {
  browser.bind(
    browser.register_service_worker(page, "/test-worker.js", True, "/"),
    fn(_ignored) {
      browser.bind(browser.call_module(page, reload_module, []), fn(_ignored) {
        browser.bind(
          browser.dispatch_keydown(page, "s", False, True),
          fn(prevented) {
            case prevented {
              False -> fn(done) { done(Error("shortcut was not prevented")) }
              True ->
                browser.bind(browser.wait(100), fn(_ignored) {
                  browser.bind(
                    browser.call_module(page, main_module, []),
                    fn(_ignored) {
                      browser.bind(browser.wait(5000), fn(_ignored) {
                        final_check(page, server)
                      })
                    },
                  )
                })
            }
          },
        )
      })
    },
  )
}

fn final_check(
  page: browser.Page,
  server: browser.Server,
) -> browser.Async(Nil) {
  case
    browser.report(page, server, [
      "svc-gateway-guest-run reload listeners installed",
      "update",
      "update found",
      "svc-gateway-guest-run main mounted",
      "client data pong",
    ])
  {
    Error(reason) -> fn(done) { done(Error(reason)) }
    Ok(Nil) ->
      browser.bind(check_view(page), fn(_ignored) {
        fn(done) {
          io.println("svc-gateway-guest-run Chromium Gleam driver passed")
          done(Ok(Nil))
        }
      })
  }
}

fn check_view(page: browser.Page) -> browser.Async(Nil) {
  browser.bind(browser.document_title(page), fn(actual_title) {
    browser.bind(browser.body_text(page), fn(actual_text) {
      browser.bind(
        browser.computed_style(page, "html", "accent-color"),
        fn(accent) {
          fn(done) {
            case
              actual_title == "Symbolize Custom"
              && actual_text |> string.contains("The Tale of Peter Rabbit")
              && accent == "rgb(255, 0, 255)"
            {
              True -> done(Ok(Nil))
              False -> done(Error("unexpected guest view"))
            }
          }
        },
      )
    })
  })
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
