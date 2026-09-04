import dev_browser_test as browser
import dev_esbuild_db as db
import gleam/bit_array
import gleam/io
import gleam/result
import gleam/string
import lib_dataflow as dataflow
import lib_hex

const database_path = "../svc-gateway-host-store/build/manifest.sqlite3"

const content_for_path_query = "test/query/content_for_path.sql"

const id_for_path_query = "test/query/id_for_path.sql"

const content_for_id_query = "test/query/content_for_id.sql"

const service_worker_path = "svc-gateway-guest-run/serviceWorkerShell.js"

const main_path = "svc-gateway-guest-run/main.mjs"

const development_path = "svc-gateway-guest-run/main.development.mjs"

type State {
  State(context: dataflow.Context, main_fetches: dataflow.Mutation(Int))
}

pub fn main() {
  let database = db.open_readonly(database_path)
  let assert Ok(shell) =
    db.query_blob_by_text(
      database,
      content_for_path_query,
      "original",
      service_worker_path,
    )
  let assert Ok(expected_main) =
    db.query_blob_by_text(
      database,
      content_for_path_query,
      "original",
      main_path,
    )
  let assert Ok(main_id) =
    db.query_blob_by_text(database, id_for_path_query, "id", main_path)
  let main_id = lib_hex.uint8_array_to_hex(main_id)
  let state = State(dataflow.dataflow(), dataflow.state(0))
  browser.run(
    fn(path, _search, _request, response) {
      handle(database, state, shell, expected_main, main_id, path, response)
    },
    fn(page, server, origin) {
      operation(page, server, origin, expected_main, main_id, state)
    },
  )
}

fn handle(
  database: db.Database,
  state: State,
  shell: BitArray,
  expected_main: BitArray,
  main_id: String,
  path: String,
  response: browser.HttpResponse,
) -> Nil {
  case path {
    "/" ->
      browser.reply_text(
        response,
        200,
        "text/html",
        "<!doctype html><title>Uncontrolled shell</title>",
      )
    "/.code/svc-gateway-guest-run/serviceWorkerShell.js" ->
      browser.reply(
        response,
        200,
        [
          #("content-type", "text/javascript"),
          #("service-worker-allowed", "/"),
        ],
        shell,
      )
    _ ->
      case string.starts_with(path, "/.code/.id/") {
        True ->
          serve_content_by_id(
            database,
            state,
            expected_main,
            main_id,
            path,
            response,
          )
        False -> browser.reply_text(response, 404, "text/plain", "")
      }
  }
}

fn serve_content_by_id(
  database: db.Database,
  state: State,
  expected_main: BitArray,
  main_id: String,
  path: String,
  response: browser.HttpResponse,
) -> Nil {
  let name = string.drop_start(path, string.length("/.code/.id/"))
  let id = case string.split_once(name, on: ".") {
    Error(Nil) -> name
    Ok(#(value, _)) -> value
  }
  case id == main_id {
    True ->
      set(
        state.context,
        state.main_fetches,
        read(state.context, state.main_fetches) + 1,
      )
    False -> Nil
  }
  case
    db.query_blob_by_blob(
      database,
      content_for_id_query,
      "original",
      lib_hex.uint8_array_from_hex(id),
    )
  {
    Error(_) ->
      browser.reply_text(response, 404, "text/plain", "missing content")
    Ok(body) -> {
      assert id != main_id || body == expected_main
      browser.reply(response, 200, [#("content-type", "text/javascript")], body)
    }
  }
}

fn operation(
  page: browser.Page,
  server: browser.Server,
  origin: String,
  expected_main: BitArray,
  main_id: String,
  state: State,
) -> browser.Async(Nil) {
  browser.bind(
    browser.register_service_worker(
      page,
      "/.code/" <> service_worker_path,
      False,
      "/",
    ),
    fn(_ignored) {
      browser.bind(browser.reload(page, True), fn(_ignored) {
        browser.bind(
          browser.fetch(page, origin <> "/.code/" <> main_path, [
            "content-type",
            "content-security-policy",
          ]),
          fn(by_path) {
            browser.bind(
              browser.fetch(page, origin <> "/.code/.id/" <> main_id <> ".mjs", [
                "content-type",
              ]),
              fn(by_id) {
                browser.bind(
                  browser.module_export_type(
                    page,
                    origin <> "/.code/" <> main_path,
                    "main",
                  ),
                  fn(imported_main) {
                    browser.bind(
                      browser.module_export_type(
                        page,
                        origin <> "/.code/" <> development_path,
                        "main",
                      ),
                      fn(imported_development) {
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
                                    expected_main,
                                    by_path,
                                    by_id,
                                    imported_main,
                                    imported_development,
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
              },
            )
          },
        )
      })
    },
  )
}

fn check(
  page: browser.Page,
  server: browser.Server,
  state: State,
  expected_main: BitArray,
  by_path: browser.FetchResult,
  by_id: browser.FetchResult,
  imported_main: String,
  imported_development: String,
  missing: browser.FetchResult,
  main: browser.FetchResult,
) -> Result(Nil, String) {
  case browser.report(page, server, []) {
    Error(reason) -> Error(reason)
    Ok(Nil) ->
      case by_path, by_id, missing, main {
        #(200, by_path_body, by_path_headers),
          #(200, by_id_body, by_id_headers),
          #(404, "Path missing from manifest", _),
          #(200, main_body, main_headers)
        -> {
          let expected_text =
            result.unwrap(bit_array.to_string(expected_main), "")
          case
            by_path_body == expected_text
            && by_id_body == expected_text
            && imported_main == "function"
            && imported_development == "function"
            && header(by_path_headers, "content-type") == "text/javascript"
            && header(by_path_headers, "content-security-policy")
            == "default-src 'self';img-src 'self' data:;style-src 'self' 'unsafe-inline'"
            && header(by_id_headers, "content-type") == "text/javascript"
            && header(main_headers, "content-type") == "text/html"
            && header(main_headers, "content-security-policy")
            == "default-src 'self';img-src 'self' data:;style-src 'self' 'unsafe-inline'"
            && string.contains(main_body, "@font-face")
            && string.contains(main_body, "@layer loader")
            && string.contains(main_body, "@layer reset")
            && read(state.context, state.main_fetches) == 1
          {
            True -> {
              io.println(
                "dev-esbuild built service worker Chromium Gleam driver passed",
              )
              Ok(Nil)
            }
            False -> Error("unexpected built service worker response")
          }
        }
        _, _, _, _ -> Error("unexpected built service worker response")
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
