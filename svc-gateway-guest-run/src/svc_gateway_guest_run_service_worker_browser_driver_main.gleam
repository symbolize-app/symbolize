import dev_browser_test as browser
import gleam/io
import gleam/string
import lib_dataflow as dataflow

const service_worker_module = "/symbolize_svc_gateway_guest_run/svc_gateway_guest_run_service_worker_browser_test_main.mjs"

type State {
  State(context: dataflow.Context, content_fetches: dataflow.Mutation(Int))
}

pub fn main() {
  let state = State(dataflow.dataflow(), dataflow.state(0))
  let root = browser.resolve_path("build/dev/javascript")
  browser.run(
    fn(path, _search, _request, response) {
      case path {
        "/test-service-worker.mjs" ->
          browser.reply_text(
            response,
            200,
            "text/javascript",
            browser.module_script(service_worker_module, "main"),
          )
        "/.code/.id/app.js" -> {
          set(
            state.context,
            state.content_fetches,
            read(state.context, state.content_fetches) + 1,
          )
          browser.wait(100)(fn(_ignored) {
            browser.reply_text(
              response,
              200,
              "text/javascript",
              "export const app = true",
            )
          })
        }
        _ -> browser.serve_static(root, path, response)
      }
    },
    fn(page, server, origin) { operation(page, server, origin, state) },
  )
}

fn operation(
  page: browser.Page,
  server: browser.Server,
  origin: String,
  state: State,
) -> browser.Async(Nil) {
  browser.bind(
    browser.register_service_worker(page, "/test-service-worker.mjs", True, "/"),
    fn(_ignored) {
      browser.bind(browser.reload(page, True), fn(_ignored) {
        browser.bind(browser.controller_state(page), fn(controller_state) {
          browser.bind(
            browser.fetch(page, origin <> "/.code/app.js", [
              "content-type",
              "content-security-policy",
            ]),
            fn(by_path) {
              browser.bind(
                browser.fetch(page, origin <> "/.code/.id/app.js", [
                  "content-type",
                ]),
                fn(by_id) {
                  browser.bind(
                    browser.fetch(page, origin <> "/.code/missing.js", []),
                    fn(missing) {
                      browser.bind(
                        browser.fetch(page, origin <> "/", [
                          "content-type",
                          "content-security-policy",
                        ]),
                        fn(main) {
                          fn(done) {
                            done(check(
                              page,
                              server,
                              state,
                              controller_state,
                              by_path,
                              by_id,
                              missing,
                              main,
                            ))
                          }
                        },
                      )
                    },
                  )
                },
              )
            },
          )
        })
      })
    },
  )
}

fn check(
  page: browser.Page,
  server: browser.Server,
  state: State,
  controller_state: String,
  by_path: browser.FetchResult,
  by_id: browser.FetchResult,
  missing: browser.FetchResult,
  main: browser.FetchResult,
) -> Result(Nil, String) {
  case browser.report(page, server, []) {
    Error(reason) -> Error(reason)
    Ok(Nil) ->
      case by_path, by_id, missing, main {
        #(200, "export const app = true", path_headers),
          #(200, "export const app = true", id_headers),
          #(404, "Path missing from manifest", _),
          #(200, main_body, main_headers)
        ->
          case
            header(path_headers, "content-type") == "text/javascript"
            && header(path_headers, "content-security-policy") == "test-policy"
            && header(id_headers, "content-type") == "text/javascript"
            && header(main_headers, "content-type") == "text/html"
            && header(main_headers, "content-security-policy") == "test-policy"
            && contains(main_body, "FONT")
            && contains(main_body, "LOADER")
            && contains(main_body, "RESET")
            && controller_state == "activated"
            && read(state.context, state.content_fetches) == 1
          {
            True -> {
              io.println(
                "svc-gateway-guest-run service worker Chromium FFI passed",
              )
              Ok(Nil)
            }
            False -> Error("unexpected service worker response")
          }
        _, _, _, _ -> Error("unexpected service worker response")
      }
  }
}

fn header(headers: List(#(String, String)), expected: String) -> String {
  case headers {
    [] -> ""
    [#(name, value), ..rest] ->
      case name == expected {
        True -> value
        False -> header(rest, expected)
      }
  }
}

fn contains(value: String, expected: String) -> Bool {
  string.contains(value, expected)
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
