import gleam/bit_array
import gleam/int
import gleam/io
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string
import lib_dataflow as dataflow

pub type Async(value) =
  fn(fn(Result(value, String)) -> Nil) -> Nil

pub fn bind(first: Async(a), next: fn(a) -> Async(b)) -> Async(b) {
  fn(done) {
    first(fn(result) {
      case result {
        Error(reason) -> done(Error(reason))
        Ok(value) -> next(value)(done)
      }
    })
  }
}

type NativeServer

pub opaque type Server {
  Server(
    native: NativeServer,
    context: dataflow.Context,
    requests: dataflow.Mutation(List(#(String, String))),
    errors: dataflow.Mutation(List(String)),
  )
}

pub type Browser

type NativePage

pub opaque type Page {
  Page(
    native: NativePage,
    context: dataflow.Context,
    logs: dataflow.Mutation(List(String)),
    errors: dataflow.Mutation(List(String)),
    failed: dataflow.Mutation(List(String)),
  )
}

pub type HttpResponse

pub type HttpRequest

pub type FetchResult =
  #(Int, String, List(#(String, String)))

@external(javascript, "./browser_ffi.mjs", "listen")
fn listen_ffi(
  handler: fn(String, String, HttpRequest, HttpResponse) -> Nil,
  on_error: fn(String) -> Nil,
  done: fn(Result(#(NativeServer, Int), String)) -> Nil,
) -> Nil

pub fn server(
  handler: fn(String, String, HttpRequest, HttpResponse) -> Nil,
) -> Async(#(Server, String)) {
  fn(done) {
    let context = dataflow.dataflow()
    let requests = dataflow.state([])
    let errors = dataflow.state([])
    listen_ffi(
      fn(path, search, request, response) {
        append_observation(context, requests, #(path, search))
        handler(path, search, request, response)
      },
      fn(error) { append_observation(context, errors, error) },
      fn(result) {
        case result {
          Error(reason) -> done(Error(reason))
          Ok(#(native, port)) ->
            done(
              Ok(#(
                Server(native, context, requests, errors),
                port_text(False, port),
              )),
            )
        }
      },
    )
  }
}

@external(javascript, "./browser_ffi.mjs", "certificate")
fn certificate_ffi(
  done: fn(Result(#(String, String, String), String)) -> Nil,
) -> Nil

@external(javascript, "./browser_ffi.mjs", "listen_secure")
fn listen_secure_ffi(
  key_path: String,
  certificate_path: String,
  handler: fn(String, String, HttpRequest, HttpResponse) -> Nil,
  on_error: fn(String) -> Nil,
  done: fn(Result(#(NativeServer, Int), String)) -> Nil,
) -> Nil

@external(javascript, "./browser_ffi.mjs", "remove_path")
fn remove_path_ffi(path: String, done: fn(Result(Nil, String)) -> Nil) -> Nil

pub fn run_secure(
  handler: fn(String, String, HttpRequest, HttpResponse) -> Nil,
  operation: fn(Page, Server, String) -> Async(Nil),
) -> Nil {
  certificate_ffi(fn(certificate_result) {
    case certificate_result {
      Error(reason) -> fail(reason)
      Ok(#(directory, key_path, certificate_path)) -> {
        let context = dataflow.dataflow()
        let requests = dataflow.state([])
        let errors = dataflow.state([])
        listen_secure_ffi(
          key_path,
          certificate_path,
          fn(path, search, request, response) {
            append_observation(context, requests, #(path, search))
            handler(path, search, request, response)
          },
          fn(error) { append_observation(context, errors, error) },
          fn(server_result) {
            case server_result {
              Error(reason) -> cleanup_directory(directory, Error(reason))
              Ok(#(native, port)) -> {
                let server = Server(native, context, requests, errors)
                let origin = port_text(True, port)
                launch(True)(fn(browser_result) {
                  case browser_result {
                    Error(reason) ->
                      finish_secure(server, directory, Error(reason))
                    Ok(browser) ->
                      new_page(browser)(fn(page_result) {
                        case page_result {
                          Error(reason) ->
                            finish_secure(server, directory, Error(reason))
                          Ok(page) ->
                            goto(page, origin <> "/")(fn(goto_result) {
                              case goto_result {
                                Error(reason) ->
                                  finish_secure(
                                    server,
                                    directory,
                                    Error(reason),
                                  )
                                Ok(Nil) ->
                                  operation(page, server, origin)(fn(result) {
                                    close_browser(browser)(fn(close_result) {
                                      case close_result {
                                        Error(reason) ->
                                          finish_secure(
                                            server,
                                            directory,
                                            Error(reason),
                                          )
                                        Ok(Nil) ->
                                          finish_secure(
                                            server,
                                            directory,
                                            result,
                                          )
                                      }
                                    })
                                  })
                              }
                            })
                        }
                      })
                  }
                })
              }
            }
          },
        )
      }
    }
  })
}

fn finish_secure(
  server: Server,
  directory: String,
  result: Result(Nil, String),
) -> Nil {
  close_server(server)(fn(close_result) {
    case close_result {
      Error(reason) -> cleanup_directory(directory, Error(reason))
      Ok(Nil) -> cleanup_directory(directory, result)
    }
  })
}

fn cleanup_directory(directory: String, result: Result(Nil, String)) -> Nil {
  remove_path_ffi(directory, fn(remove_result) {
    case result, remove_result {
      Error(reason), _ -> fail(reason)
      Ok(Nil), Error(reason) -> fail(reason)
      Ok(Nil), Ok(Nil) -> Nil
    }
  })
}

@external(javascript, "./browser_ffi.mjs", "close_server")
fn close_server_ffi(
  server: NativeServer,
  done: fn(Result(Nil, String)) -> Nil,
) -> Nil

pub fn close_server(server: Server) -> Async(Nil) {
  fn(done) {
    let Server(native, _, _, _) = server
    close_server_ffi(native, done)
  }
}

@external(javascript, "./browser_ffi.mjs", "read_file")
fn read_file_ffi(path: String, done: fn(Result(BitArray, String)) -> Nil) -> Nil

pub fn read_file(path: String) -> Async(BitArray) {
  fn(done) { read_file_ffi(path, done) }
}

@external(javascript, "./browser_ffi.mjs", "read_request_body")
fn read_request_body_ffi(
  request: HttpRequest,
  on_chunk: fn(String) -> Nil,
  done: fn(Result(String, String)) -> Nil,
) -> Nil

pub fn read_request_body(request: HttpRequest) -> Async(String) {
  fn(done) {
    let context = dataflow.dataflow()
    let chunks = dataflow.state([])
    read_request_body_ffi(
      request,
      fn(chunk) { append_observation(context, chunks, chunk) },
      fn(result) {
        case result {
          Error(reason) -> done(Error(reason))
          Ok(_) -> done(Ok(join_chunks(read_observation(chunks))))
        }
      },
    )
  }
}

@external(javascript, "./browser_ffi.mjs", "resolve_path")
pub fn resolve_path(path: String) -> String

@external(javascript, "./browser_ffi.mjs", "reply")
fn reply_ffi(
  response: HttpResponse,
  status: Int,
  headers: List(#(String, String)),
  body: BitArray,
) -> Nil

@external(javascript, "./browser_ffi.mjs", "start_response")
fn start_response_ffi(
  response: HttpResponse,
  status: Int,
  headers: List(#(String, String)),
) -> Nil

pub fn reply(
  response: HttpResponse,
  status: Int,
  headers: List(#(String, String)),
  body: BitArray,
) -> Nil {
  reply_ffi(response, status, headers, body)
}

pub fn reply_text(
  response: HttpResponse,
  status: Int,
  content_type: String,
  body: String,
) -> Nil {
  reply(
    response,
    status,
    [#("content-type", content_type)],
    bit_array.from_string(body),
  )
}

pub fn start_response(
  response: HttpResponse,
  status: Int,
  headers: List(#(String, String)),
) -> Nil {
  start_response_ffi(response, status, headers)
}

@external(javascript, "./browser_ffi.mjs", "write")
pub fn write(response: HttpResponse, body: String) -> Nil

@external(javascript, "./browser_ffi.mjs", "end")
pub fn end(response: HttpResponse, body: String) -> Nil

pub fn server_request_log(server: Server) -> List(#(String, String)) {
  let Server(_, _, requests, _) = server
  read_observation(requests)
}

pub fn server_errors(server: Server) -> List(String) {
  let Server(_, _, _, errors) = server
  read_observation(errors)
}

pub fn serve_static(
  root: String,
  pathname: String,
  response: HttpResponse,
) -> Nil {
  case pathname {
    "/" ->
      reply_text(
        response,
        200,
        "text/html",
        "<!doctype html><title>Gleam browser test</title>",
      )
    _ -> {
      let relative = string.drop_start(pathname, 1)
      let filename = resolve_path(root <> "/" <> relative)
      case string.starts_with(filename, root <> "/") {
        False -> reply_text(response, 404, "text/plain", "")
        True ->
          read_file(filename)(fn(result) {
            case result {
              Error(_) -> reply_text(response, 404, "text/plain", "")
              Ok(body) ->
                reply(
                  response,
                  200,
                  [#("content-type", content_type(filename))],
                  body,
                )
            }
          })
      }
    }
  }
}

fn content_type(path: String) -> String {
  case string.ends_with(path, ".mjs") {
    True -> "text/javascript"
    False -> "application/octet-stream"
  }
}

@external(javascript, "./browser_ffi.mjs", "launch")
fn launch_ffi(done: fn(Result(Browser, String)) -> Nil) -> Nil

@external(javascript, "./browser_ffi.mjs", "launch_insecure")
fn launch_insecure_ffi(done: fn(Result(Browser, String)) -> Nil) -> Nil

pub fn launch(ignore_certificate_errors: Bool) -> Async(Browser) {
  fn(done) {
    case ignore_certificate_errors {
      True -> launch_insecure_ffi(done)
      False -> launch_ffi(done)
    }
  }
}

@external(javascript, "./browser_ffi.mjs", "new_page")
fn new_page_ffi(
  browser: Browser,
  on_console: fn(String) -> Nil,
  on_error: fn(String) -> Nil,
  on_failed: fn(String) -> Nil,
  done: fn(Result(NativePage, String)) -> Nil,
) -> Nil

pub fn new_page(browser: Browser) -> Async(Page) {
  fn(done) {
    let context = dataflow.dataflow()
    let logs = dataflow.state([])
    let errors = dataflow.state([])
    let failed = dataflow.state([])
    new_page_ffi(
      browser,
      fn(message) { append_observation(context, logs, message) },
      fn(error) { append_observation(context, errors, error) },
      fn(request) { append_observation(context, failed, request) },
      fn(result) {
        case result {
          Error(reason) -> done(Error(reason))
          Ok(native) -> done(Ok(Page(native, context, logs, errors, failed)))
        }
      },
    )
  }
}

@external(javascript, "./browser_ffi.mjs", "close_browser")
fn close_browser_ffi(
  browser: Browser,
  done: fn(Result(Nil, String)) -> Nil,
) -> Nil

pub fn close_browser(browser: Browser) -> Async(Nil) {
  fn(done) { close_browser_ffi(browser, done) }
}

@external(javascript, "./browser_ffi.mjs", "goto")
fn goto_ffi(
  page: NativePage,
  url: String,
  done: fn(Result(Nil, String)) -> Nil,
) -> Nil

pub fn goto(page: Page, url: String) -> Async(Nil) {
  fn(done) { goto_ffi(page_native(page), url, done) }
}

@external(javascript, "./browser_ffi.mjs", "reload")
fn reload_ffi(
  page: NativePage,
  network_idle: Bool,
  done: fn(Result(Nil, String)) -> Nil,
) -> Nil

pub fn reload(page: Page, network_idle: Bool) -> Async(Nil) {
  fn(done) { reload_ffi(page_native(page), network_idle, done) }
}

@external(javascript, "./browser_ffi.mjs", "call_module")
fn call_module_ffi(
  page: NativePage,
  path: String,
  args: List(String),
  done: fn(Result(Nil, String)) -> Nil,
) -> Nil

pub fn call_module(page: Page, path: String, args: List(String)) -> Async(Nil) {
  fn(done) { call_module_ffi(page_native(page), path, args, done) }
}

@external(javascript, "./browser_ffi.mjs", "module_export_type")
fn module_export_type_ffi(
  page: NativePage,
  path: String,
  name: String,
  done: fn(Result(String, String)) -> Nil,
) -> Nil

pub fn module_export_type(
  page: Page,
  path: String,
  name: String,
) -> Async(String) {
  fn(done) { module_export_type_ffi(page_native(page), path, name, done) }
}

@external(javascript, "./browser_ffi.mjs", "wait")
fn wait_ffi(milliseconds: Int, done: fn() -> Nil) -> Nil

pub fn wait(milliseconds: Int) -> Async(Nil) {
  fn(done) { wait_ffi(milliseconds, fn() { done(Ok(Nil)) }) }
}

pub fn console_messages(page: Page) -> List(String) {
  let Page(_, _, logs, _, _) = page
  read_observation(logs)
}

pub fn page_errors(page: Page) -> List(String) {
  let Page(_, _, _, errors, _) = page
  read_observation(errors)
}

pub fn failed_requests(page: Page) -> List(String) {
  let Page(_, _, _, _, failed) = page
  read_observation(failed)
}

pub fn contains_message(messages: List(String), expected: String) -> Bool {
  list.contains(messages, expected)
}

pub fn join_messages(values: List(String)) -> String {
  join(values)
}

pub fn report(
  page: Page,
  server: Server,
  expected_messages: List(String),
) -> Result(Nil, String) {
  case page_errors(page) {
    [] -> {
      case server_errors(server) {
        [] -> check_messages(console_messages(page), expected_messages)
        errors -> Error("server errors: " <> join(errors))
      }
    }
    errors -> Error("page errors: " <> join(errors))
  }
}

fn check_messages(
  messages: List(String),
  expected: List(String),
) -> Result(Nil, String) {
  case expected {
    [] -> Ok(Nil)
    [first, ..rest] ->
      case list.contains(messages, first) {
        True -> check_messages(messages, rest)
        False ->
          Error(
            "missing browser message: "
            <> first
            <> "\nconsole="
            <> join(messages),
          )
      }
  }
}

fn join(values: List(String)) -> String {
  case values {
    [] -> ""
    [first, ..rest] -> join_loop(rest, first)
  }
}

fn join_loop(values: List(String), output: String) -> String {
  case values {
    [] -> output
    [first, ..rest] -> join_loop(rest, output <> ", " <> first)
  }
}

fn join_chunks(values: List(String)) -> String {
  case values {
    [] -> ""
    [first, ..rest] -> first <> join_chunks(rest)
  }
}

@external(javascript, "./browser_ffi.mjs", "fetch")
fn fetch_ffi(
  page: NativePage,
  url: String,
  header_names: List(String),
  done: fn(Result(FetchResult, String)) -> Nil,
) -> Nil

pub fn fetch(
  page: Page,
  url: String,
  header_names: List(String),
) -> Async(FetchResult) {
  fn(done) { fetch_ffi(page_native(page), url, header_names, done) }
}

@external(javascript, "./browser_ffi.mjs", "document_title")
fn document_title_ffi(page: NativePage, done: fn(String) -> Nil) -> Nil

pub fn document_title(page: Page) -> Async(String) {
  fn(done) {
    document_title_ffi(page_native(page), fn(value) { done(Ok(value)) })
  }
}

@external(javascript, "./browser_ffi.mjs", "body_text")
fn body_text_ffi(page: NativePage, done: fn(String) -> Nil) -> Nil

pub fn body_text(page: Page) -> Async(String) {
  fn(done) { body_text_ffi(page_native(page), fn(value) { done(Ok(value)) }) }
}

@external(javascript, "./browser_ffi.mjs", "body_children")
fn body_children_ffi(page: NativePage, done: fn(List(String)) -> Nil) -> Nil

pub fn body_children(page: Page) -> Async(List(String)) {
  fn(done) {
    body_children_ffi(page_native(page), fn(value) { done(Ok(value)) })
  }
}

@external(javascript, "./browser_ffi.mjs", "element_text")
fn element_text_ffi(
  page: NativePage,
  selector: String,
  done: fn(Result(Option(String), String)) -> Nil,
) -> Nil

pub fn element_text(page: Page, selector: String) -> Async(String) {
  fn(done) {
    element_text_ffi(page_native(page), selector, fn(result) {
      done(result |> result_required("missing element: " <> selector))
    })
  }
}

@external(javascript, "./browser_ffi.mjs", "element_attribute")
fn element_attribute_ffi(
  page: NativePage,
  selector: String,
  name: String,
  done: fn(Result(Option(String), String)) -> Nil,
) -> Nil

pub fn element_attribute(
  page: Page,
  selector: String,
  name: String,
) -> Async(String) {
  fn(done) {
    element_attribute_ffi(page_native(page), selector, name, fn(result) {
      done(
        result
        |> result_required(
          "missing attribute: " <> selector <> "[" <> name <> "]",
        ),
      )
    })
  }
}

@external(javascript, "./browser_ffi.mjs", "element_value")
fn element_value_ffi(
  page: NativePage,
  selector: String,
  done: fn(Result(Option(String), String)) -> Nil,
) -> Nil

pub fn element_value(page: Page, selector: String) -> Async(String) {
  fn(done) {
    element_value_ffi(page_native(page), selector, fn(result) {
      done(result |> result_required("missing value element: " <> selector))
    })
  }
}

@external(javascript, "./browser_ffi.mjs", "element_checked")
fn element_checked_ffi(
  page: NativePage,
  selector: String,
  done: fn(Result(Option(Bool), String)) -> Nil,
) -> Nil

pub fn element_checked(page: Page, selector: String) -> Async(Bool) {
  fn(done) {
    element_checked_ffi(page_native(page), selector, fn(result) {
      done(result |> result_required("missing checked element: " <> selector))
    })
  }
}

@external(javascript, "./browser_ffi.mjs", "element_namespace")
fn element_namespace_ffi(
  page: NativePage,
  selector: String,
  done: fn(Result(Option(String), String)) -> Nil,
) -> Nil

pub fn element_namespace(page: Page, selector: String) -> Async(String) {
  fn(done) {
    element_namespace_ffi(page_native(page), selector, fn(result) {
      done(result |> result_required("missing namespace element: " <> selector))
    })
  }
}

@external(javascript, "./browser_ffi.mjs", "computed_style")
fn computed_style_ffi(
  page: NativePage,
  selector: String,
  property: String,
  done: fn(Result(Option(String), String)) -> Nil,
) -> Nil

pub fn computed_style(
  page: Page,
  selector: String,
  property: String,
) -> Async(String) {
  fn(done) {
    computed_style_ffi(page_native(page), selector, property, fn(result) {
      done(result |> result_required("missing style element: " <> selector))
    })
  }
}

fn result_required(
  value: Result(Option(a), String),
  missing: String,
) -> Result(a, String) {
  case value {
    Error(reason) -> Error(reason)
    Ok(Some(value)) -> Ok(value)
    Ok(None) -> Error(missing)
  }
}

@external(javascript, "./browser_ffi.mjs", "click")
fn click_ffi(
  page: NativePage,
  selector: String,
  done: fn(Result(Nil, String)) -> Nil,
) -> Nil

pub fn click(page: Page, selector: String) -> Async(Nil) {
  fn(done) { click_ffi(page_native(page), selector, done) }
}

@external(javascript, "./browser_ffi.mjs", "dispatch_keydown")
fn dispatch_keydown_ffi(
  page: NativePage,
  key: String,
  meta: Bool,
  ctrl: Bool,
  done: fn(Result(Bool, String)) -> Nil,
) -> Nil

pub fn dispatch_keydown(
  page: Page,
  key: String,
  meta: Bool,
  ctrl: Bool,
) -> Async(Bool) {
  fn(done) { dispatch_keydown_ffi(page_native(page), key, meta, ctrl, done) }
}

@external(javascript, "./browser_ffi.mjs", "register_service_worker_module")
fn register_service_worker_module_ffi(
  page: NativePage,
  path: String,
  scope: String,
  done: fn(Result(Nil, String)) -> Nil,
) -> Nil

@external(javascript, "./browser_ffi.mjs", "register_service_worker_classic")
fn register_service_worker_classic_ffi(
  page: NativePage,
  path: String,
  scope: String,
  done: fn(Result(Nil, String)) -> Nil,
) -> Nil

pub fn register_service_worker(
  page: Page,
  path: String,
  module: Bool,
  scope: String,
) -> Async(Nil) {
  fn(done) {
    case module {
      True ->
        register_service_worker_module_ffi(page_native(page), path, scope, done)
      False ->
        register_service_worker_classic_ffi(
          page_native(page),
          path,
          scope,
          done,
        )
    }
  }
}

@external(javascript, "./browser_ffi.mjs", "controller_state")
fn controller_state_ffi(
  page: NativePage,
  done: fn(Result(String, String)) -> Nil,
) -> Nil

pub fn controller_state(page: Page) -> Async(String) {
  fn(done) { controller_state_ffi(page_native(page), done) }
}

pub fn module_script(path: String, export_name: String) -> String {
  "import { "
  <> export_name
  <> " } from "
  <> json_string(path)
  <> "; "
  <> export_name
  <> "()"
}

fn json_string(value: String) -> String {
  "\""
  <> value
  |> string.replace(each: "\\", with: "\\\\")
  |> string.replace(each: "\"", with: "\\\"")
  <> "\""
}

pub fn run(
  handler: fn(String, String, HttpRequest, HttpResponse) -> Nil,
  operation: fn(Page, Server, String) -> Async(Nil),
) -> Nil {
  server(handler)(fn(server_result) {
    case server_result {
      Error(reason) -> fail(reason)
      Ok(#(server, origin)) ->
        launch(False)(fn(browser_result) {
          case browser_result {
            Error(reason) -> finish_server(server, Error(reason))
            Ok(browser) ->
              new_page(browser)(fn(page_result) {
                case page_result {
                  Error(reason) ->
                    finish_browser(browser, server, Error(reason))
                  Ok(page) ->
                    goto(page, origin <> "/")(fn(goto_result) {
                      case goto_result {
                        Error(reason) ->
                          finish_browser(browser, server, Error(reason))
                        Ok(Nil) ->
                          operation(page, server, origin)(fn(test_result) {
                            finish_browser(browser, server, test_result)
                          })
                      }
                    })
                }
              })
          }
        })
    }
  })
}

fn finish_browser(
  browser: Browser,
  server: Server,
  result: Result(Nil, String),
) -> Nil {
  close_browser(browser)(fn(close_result) {
    case close_result {
      Error(reason) -> finish_server(server, Error(reason))
      Ok(Nil) -> finish_server(server, result)
    }
  })
}

fn finish_server(server: Server, result: Result(Nil, String)) -> Nil {
  close_server(server)(fn(close_result) {
    case result, close_result {
      Error(reason), _ ->
        fail(
          reason
          <> "\nrequests="
          <> join_messages(requests_as_strings(server_request_log(server))),
        )
      Ok(Nil), Error(reason) -> fail(reason)
      Ok(Nil), Ok(Nil) -> Nil
    }
  })
}

fn page_native(page: Page) -> NativePage {
  let Page(native, _, _, _, _) = page
  native
}

fn append_observation(
  context: dataflow.Context,
  state: dataflow.Mutation(List(value)),
  value: value,
) -> Nil {
  let current =
    dataflow.value(dataflow.to_computation(dataflow.mutation(state)))
  let assert Ok(Nil) =
    dataflow.txn(context, fn() {
      dataflow.set(context, state, list.append(current, [value]))
    })
  Nil
}

fn read_observation(state: dataflow.Mutation(value)) -> value {
  dataflow.value(dataflow.to_computation(dataflow.mutation(state)))
}

fn port_text(secure: Bool, port: Int) -> String {
  let scheme = case secure {
    True -> "https"
    False -> "http"
  }
  scheme <> "://127.0.0.1:" <> int.to_string(port)
}

fn fail(reason: String) -> Nil {
  io.println(reason)
  set_failure()
}

fn requests_as_strings(requests: List(#(String, String))) -> List(String) {
  case requests {
    [] -> []
    [#(path, search), ..rest] -> [path <> search, ..requests_as_strings(rest)]
  }
}

@external(javascript, "./browser_ffi.mjs", "set_failure")
fn set_failure() -> Nil
