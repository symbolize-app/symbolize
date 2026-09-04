import dev_browser_test as browser
import dev_esbuild_db as db
import gleam/io
import gleam/string

const database_path = "../svc-gateway-host-store/build/manifest.sqlite3"

const content_for_path_query = "test/query/content_for_path.sql"

const entry_path = "/.code/svc-gateway-guest-run/main.development.mjs"

pub fn main() {
  let database = db.open_readonly(database_path)
  let root = browser.resolve_path("build/dev/javascript")
  browser.run(
    fn(path, _search, _request, response) {
      case path {
        "/" ->
          browser.reply_text(
            response,
            200,
            "text/html",
            "<!doctype html><title>Gleam development graph</title>",
          )
        _ ->
          case string.starts_with(path, "/.code/") {
            True -> serve(database, path, response)
            False -> browser.serve_static(root, path, response)
          }
      }
    },
    operation,
  )
}

fn serve(
  database: db.Database,
  path: String,
  response: browser.HttpResponse,
) -> Nil {
  let content_path = string.drop_start(path, string.length("/.code/"))
  case
    db.query_blob_by_text(
      database,
      content_for_path_query,
      "original",
      content_path,
    )
  {
    Error(_) ->
      browser.reply_text(
        response,
        404,
        "text/plain",
        "missing " <> content_path,
      )
    Ok(body) ->
      browser.reply(response, 200, [#("content-type", "text/javascript")], body)
  }
}

fn operation(
  page: browser.Page,
  server: browser.Server,
  origin: String,
) -> browser.Async(Nil) {
  browser.bind(
    browser.call_module(page, origin <> entry_path, []),
    fn(_ignored) {
      browser.bind(browser.wait(2000), fn(_ignored) {
        fn(done) {
          case
            browser.report(page, server, [
              "lib-collection Gleam parity tests passed",
              "lib-dataflow explicit Gleam parity slice passed",
              "lib-dataflow async callback parity passed",
              "lib-dataflow async error parity passed",
              "lib-dataflow async multi-value parity passed",
              "lib-dataflow async epoch cache parity passed",
              "lib-dataflow async multi-input cache parity passed",
              "lib-dataflow async state/effect parity passed",
              "lib-dataflow async rejection propagation passed",
              "lib-dataflow async derived/effect arity parity passed",
              "lib-dataflow async map state epoch parity passed",
              "lib-dataflow async map queued epochs parity passed",
              "lib-error Gleam parity tests passed",
              "lib-error abort retry passed",
              "lib-hex Gleam parity tests passed",
              "lib-markup Chromium DOM FFI passed",
              "lib-payload Gleam parity tests passed",
              "lib-random Gleam parity tests passed",
              "lib-stream source timeout parity passed",
              "lib-stream sink Web Streams parity passed",
              "lib-stream source Web Streams parity passed",
              "lib-styling Gleam parity tests passed",
              "lib-time custom abort race passed",
              "lib-time Gleam parity tests passed",
              "lib-time system timer callback passed",
              "lib-time abort race passed",
              "lib-time delay parity passed",
              "development Gleam test graph loaded",
            ])
          {
            Error(reason) -> done(Error(reason))
            Ok(Nil) -> {
              io.println(
                "dev-esbuild development graph Chromium Gleam driver passed",
              )
              done(Ok(Nil))
            }
          }
        }
      })
    },
  )
}
