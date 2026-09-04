import gleam/dict
import gleam/int
import gleam/io
import gleam/option.{type Option, None, Some}
import gleam/string
import lib_collection
import lib_dataflow as dataflow
import lib_time
import svc_gateway_guest_run_service_worker as rules

const cache_name = "code-v1"

const code_id_prefix = "/.code/.id/"

pub type Config {
  Config(
    version: Int,
    manifest: List(#(String, String)),
    content_security_policy: String,
    font_css: String,
    loader_css: String,
    reset_css: String,
    main_html: String,
  )
}

pub type ServiceWorker

pub type Cache

pub type FetchEvent

pub type Request

pub type Response

@external(javascript, "./service_worker_ffi.mjs", "service_worker")
fn service_worker() -> ServiceWorker

@external(javascript, "./service_worker_ffi.mjs", "cache_promise")
fn cache_promise(name: String) -> Cache

@external(javascript, "./service_worker_ffi.mjs", "listen_message")
fn listen_message(worker: ServiceWorker, callback: fn(String) -> Nil) -> Nil

@external(javascript, "./service_worker_ffi.mjs", "listen_install")
fn listen_install(worker: ServiceWorker, callback: fn() -> Nil) -> Nil

@external(javascript, "./service_worker_ffi.mjs", "listen_activate")
fn listen_activate(worker: ServiceWorker, callback: fn() -> Nil) -> Nil

@external(javascript, "./service_worker_ffi.mjs", "listen_fetch")
fn listen_fetch(
  worker: ServiceWorker,
  callback: fn(FetchEvent, Bool, String, String) -> Nil,
) -> Nil

@external(javascript, "./service_worker_ffi.mjs", "skip_waiting")
fn skip_waiting(worker: ServiceWorker) -> Nil

@external(javascript, "./service_worker_ffi.mjs", "respond_with")
fn respond_with(
  event: FetchEvent,
  produce: fn(fn(Response) -> Nil, fn(String) -> Nil) -> Nil,
) -> Nil

@external(javascript, "./service_worker_ffi.mjs", "new_request")
fn new_request(url: String) -> Request

@external(javascript, "./service_worker_ffi.mjs", "fetch_request")
fn fetch_request(
  request: Request,
  on_success: fn(Response) -> Nil,
  on_failure: fn(String) -> Nil,
) -> Nil

@external(javascript, "./service_worker_ffi.mjs", "response_ok")
fn response_ok(response: Response) -> Bool

@external(javascript, "./service_worker_ffi.mjs", "response_clone")
fn response_clone(response: Response) -> Response

@external(javascript, "./service_worker_ffi.mjs", "response_with_headers")
fn response_with_headers(
  response: Response,
  content_security_policy: String,
  content_type: String,
) -> Response

@external(javascript, "./service_worker_ffi.mjs", "response_text")
fn response_text(
  body: String,
  content_security_policy: String,
  content_type: String,
) -> Response

@external(javascript, "./service_worker_ffi.mjs", "not_found")
fn not_found(body: String) -> Response

@external(javascript, "./service_worker_ffi.mjs", "cache_match")
fn cache_match(
  cache: Cache,
  request: Request,
  done: fn(Result(Option(Response), String)) -> Nil,
) -> Nil

@external(javascript, "./service_worker_ffi.mjs", "cache_put")
fn cache_put(
  cache: Cache,
  request: Request,
  response: Response,
  on_success: fn() -> Nil,
  on_failure: fn(String) -> Nil,
) -> Nil

@external(javascript, "./service_worker_ffi.mjs", "cache_keys")
fn cache_keys(
  cache: Cache,
  on_keys: fn(List(Request)) -> Nil,
  on_done: fn() -> Nil,
  on_failure: fn(String) -> Nil,
) -> Nil

@external(javascript, "./service_worker_ffi.mjs", "request_method")
fn request_method(request: Request) -> String

@external(javascript, "./service_worker_ffi.mjs", "request_same_origin")
fn request_same_origin(request: Request) -> Bool

@external(javascript, "./service_worker_ffi.mjs", "request_path")
fn request_path(request: Request) -> String

@external(javascript, "./service_worker_ffi.mjs", "cache_delete")
fn cache_delete(
  cache: Cache,
  request: Request,
  on_success: fn() -> Nil,
  on_failure: fn(String) -> Nil,
) -> Nil

@external(javascript, "./service_worker_ffi.mjs", "match_clients")
fn match_clients(
  worker: ServiceWorker,
  on_clients: fn(List(Client)) -> Nil,
  on_done: fn() -> Nil,
  on_failure: fn(String) -> Nil,
) -> Nil

@external(javascript, "./service_worker_ffi.mjs", "client_is_window")
fn client_is_window(client: Client) -> Bool

@external(javascript, "./service_worker_ffi.mjs", "post_message")
fn post_message(client: Client, value: String) -> Nil

pub type Client

type Waiter {
  Waiter(resolve: fn(Response) -> Nil, reject: fn(String) -> Nil)
}

type MemoEntry {
  Pending(List(Waiter))
  Ready(Response)
}

type Runtime {
  Runtime(
    worker: ServiceWorker,
    cache: Cache,
    config: Config,
    time: lib_time.Context,
    dataflow: dataflow.Context,
    memo: dataflow.Mutation(List(#(String, MemoEntry))),
  )
}

pub fn main(config: Config) -> Nil {
  let worker = service_worker()
  let cache = cache_promise(cache_name)
  let time = lib_time.new_context(lib_time.time())
  let dataflow_context = dataflow.dataflow()
  let memo: dataflow.Mutation(List(#(String, MemoEntry))) = dataflow.state([])
  let Config(version, ..) = config
  io.println(int.to_string(version) <> " loading")
  let runtime =
    Runtime(
      worker: worker,
      cache: cache,
      config: config,
      time: time,
      dataflow: dataflow_context,
      memo: memo,
    )
  install(runtime)
}

fn install(runtime: Runtime) -> Nil {
  let Runtime(worker, _, config, time, _, _) = runtime
  let Config(version, ..) = config
  listen_message(worker, fn(data) {
    io.println(int.to_string(version) <> " message " <> data)
  })
  listen_install(worker, fn() {
    io.println(int.to_string(version) <> " install")
    skip_waiting(worker)
  })
  listen_activate(worker, fn() {
    io.println(int.to_string(version) <> " activate")
    reset_clients(runtime)
    lib_time.delay(time, 5000.0, fn() { prepare_cache(runtime) })
  })
  listen_fetch(worker, fn(event, same_origin, method, path) {
    case same_origin && method == "GET" {
      True -> handle(runtime, event, path)
      False -> Nil
    }
  })
}

fn handle(runtime: Runtime, event: FetchEvent, path: String) -> Nil {
  case rules.route(path) {
    rules.ContentById(content_id) ->
      respond_with(event, fn(resolve, reject) {
        fetch_content_by_id(runtime, content_id, resolve, reject)
      })
    rules.ContentByPath(content_path) ->
      respond_with(event, fn(resolve, reject) {
        fetch_content_by_path(runtime, content_path, resolve, reject)
      })
    rules.MainHtml ->
      respond_with(event, fn(resolve, reject) {
        patch_main_html(runtime, resolve, reject)
      })
  }
}

fn fetch_content_by_path(
  runtime: Runtime,
  path: String,
  resolve: fn(Response) -> Nil,
  reject: fn(String) -> Nil,
) -> Nil {
  let Runtime(_, _, Config(version, manifest, ..), _, _, _) = runtime
  case find_manifest(manifest, path) {
    None -> {
      io.println(int.to_string(version) <> " manifest error " <> path)
      resolve(not_found("Path missing from manifest"))
    }
    Some(content_id) ->
      fetch_content_by_id(runtime, content_id, resolve, reject)
  }
}

fn fetch_content_by_id(
  runtime: Runtime,
  content_id: String,
  resolve: fn(Response) -> Nil,
  reject: fn(String) -> Nil,
) -> Nil {
  let Runtime(_, _, _, _, dataflow, memo) = runtime
  let current = dataflow.value(dataflow.to_computation(dataflow.mutation(memo)))
  case memo_find(current, content_id) {
    Some(Ready(response)) ->
      deliver(runtime, content_id, response, resolve, reject)
    Some(Pending(waiters)) -> {
      let next =
        memo_replace(
          current,
          content_id,
          Pending(append_waiter(waiters, Waiter(resolve, reject))),
        )
      let assert Ok(Nil) =
        dataflow.txn(dataflow, fn() { dataflow.set(dataflow, memo, next) })
      Nil
    }
    None -> {
      let next =
        memo_replace(
          current,
          content_id,
          Pending([Waiter(resolve: resolve, reject: reject)]),
        )
      let assert Ok(Nil) =
        dataflow.txn(dataflow, fn() { dataflow.set(dataflow, memo, next) })
      begin_fetch(runtime, content_id)
    }
  }
  Nil
}

fn begin_fetch(runtime: Runtime, content_id: String) -> Nil {
  let Runtime(_, cache, Config(version, _, _, ..), _, _, _) = runtime
  let request = new_request(code_id_prefix <> content_id)
  cache_match(cache, request, fn(result) {
    case result {
      Error(reason) -> finish_fetch_error(runtime, content_id, reason)
      Ok(Some(response)) -> finish_fetch(runtime, content_id, response, False)
      Ok(None) -> {
        io.println(int.to_string(version) <> " cache miss " <> content_id)
        fetch_request(
          request,
          fn(response) {
            case response_ok(response) {
              True ->
                cache_put(
                  cache,
                  request,
                  response_clone(response),
                  fn() { finish_fetch(runtime, content_id, response, True) },
                  fn(reason) { finish_fetch_error(runtime, content_id, reason) },
                )
              False -> finish_fetch(runtime, content_id, response, False)
            }
          },
          fn(reason) { finish_fetch_error(runtime, content_id, reason) },
        )
      }
    }
  })
}

fn finish_fetch(
  runtime: Runtime,
  content_id: String,
  response: Response,
  keep: Bool,
) -> Nil {
  let Runtime(_, _, _, _, dataflow, memo) = runtime
  let current = dataflow.value(dataflow.to_computation(dataflow.mutation(memo)))
  case memo_find(current, content_id) {
    Some(Pending(waiters)) -> {
      let next = case keep {
        True -> memo_replace(current, content_id, Ready(response))
        False -> memo_remove(current, content_id)
      }
      let assert Ok(Nil) =
        dataflow.txn(dataflow, fn() { dataflow.set(dataflow, memo, next) })
      notify_success(runtime, content_id, response, waiters)
    }
    _ -> Nil
  }
}

fn finish_fetch_error(
  runtime: Runtime,
  content_id: String,
  reason: String,
) -> Nil {
  let Runtime(_, _, _, _, dataflow, memo) = runtime
  let current = dataflow.value(dataflow.to_computation(dataflow.mutation(memo)))
  case memo_find(current, content_id) {
    Some(Pending(waiters)) -> {
      let assert Ok(Nil) =
        dataflow.txn(dataflow, fn() {
          dataflow.set(dataflow, memo, memo_remove(current, content_id))
        })
      notify_failure(waiters, reason)
    }
    _ -> Nil
  }
}

fn notify_success(
  runtime: Runtime,
  content_id: String,
  response: Response,
  waiters: List(Waiter),
) -> Nil {
  case waiters {
    [] -> Nil
    [Waiter(resolve, reject), ..rest] -> {
      deliver(runtime, content_id, response, resolve, reject)
      notify_success(runtime, content_id, response, rest)
    }
  }
}

fn notify_failure(waiters: List(Waiter), reason: String) -> Nil {
  case waiters {
    [] -> Nil
    [Waiter(_, reject), ..rest] -> {
      reject(reason)
      notify_failure(rest, reason)
    }
  }
}

fn deliver(
  runtime: Runtime,
  content_id: String,
  response: Response,
  resolve: fn(Response) -> Nil,
  reject: fn(String) -> Nil,
) -> Nil {
  let Runtime(_, _, Config(_, _, content_security_policy, ..), _, _, _) =
    runtime
  case rules.content_type(content_id) {
    Ok(content_type) ->
      resolve(response_with_headers(
        response,
        string.trim_end(content_security_policy),
        content_type,
      ))
    Error(reason) -> reject(reason)
  }
}

fn patch_main_html(
  runtime: Runtime,
  resolve: fn(Response) -> Nil,
  reject: fn(String) -> Nil,
) -> Nil {
  let Runtime(
    _,
    _,
    Config(_, _, content_security_policy, font, loader, reset, main_html),
    _,
    _,
    _,
  ) = runtime
  let body =
    lib_collection.apply_template(
      lib_collection.css_import_pattern(),
      main_html,
      dict.from_list([
        #("font", font),
        #("loader", loader),
        #("reset", reset),
      ]),
    )
  case rules.content_type("main.html") {
    Ok(content_type) ->
      resolve(response_text(
        body,
        string.trim_end(content_security_policy),
        content_type,
      ))
    Error(reason) -> reject(reason)
  }
}

fn reset_clients(runtime: Runtime) -> Nil {
  let Runtime(worker, ..) = runtime
  match_clients(
    worker,
    fn(clients) { reset_client_list(clients) },
    fn() { Nil },
    fn(reason) { io.println("client reset error " <> reason) },
  )
}

fn reset_client_list(clients: List(Client)) -> Nil {
  case clients {
    [] -> Nil
    [client, ..rest] -> {
      case client_is_window(client) {
        True -> post_message(client, "reload")
        False -> Nil
      }
      reset_client_list(rest)
    }
  }
}

fn prepare_cache(runtime: Runtime) -> Nil {
  let Runtime(_, cache, Config(_, manifest, ..), _, context, _) = runtime
  let remaining = dataflow.state(manifest_values(manifest))
  cache_keys(
    cache,
    fn(requests) { prepare_cache_keys(runtime, context, remaining, requests) },
    fn() {
      let paths =
        dataflow.value(dataflow.to_computation(dataflow.mutation(remaining)))
      prepare_paths(runtime, paths)
    },
    fn(reason) { io.println("cache keys error " <> reason) },
  )
}

fn prepare_cache_keys(
  runtime: Runtime,
  context: dataflow.Context,
  remaining: dataflow.Mutation(List(String)),
  requests: List(Request),
) -> Nil {
  case requests {
    [] -> Nil
    [request, ..rest] -> {
      let method = request_method(request)
      let same_origin = request_same_origin(request)
      let path = request_path(request)
      let content = case same_origin && method == "GET" {
        True ->
          non_empty(lib_collection.strip_prefix(
            path,
            lib_collection.prefix(code_id_prefix),
          ))
        False -> None
      }
      case content {
        Some(content_path) -> {
          let current =
            dataflow.value(
              dataflow.to_computation(dataflow.mutation(remaining)),
            )
          case remove_value(current, content_path) {
            Some(next) -> {
              let assert Ok(Nil) =
                dataflow.txn(context, fn() {
                  dataflow.set(context, remaining, next)
                })
              Nil
            }
            None -> evict(runtime, request, method, path)
          }
        }
        None -> evict(runtime, request, method, path)
      }
      prepare_cache_keys(runtime, context, remaining, rest)
    }
  }
}

fn prepare_paths(runtime: Runtime, paths: List(String)) -> Nil {
  case paths {
    [] -> Nil
    [path, ..rest] -> {
      let Runtime(_, _, Config(version, ..), _, _, _) = runtime
      io.println(int.to_string(version) <> " prepare " <> path)
      fetch_content_by_id(
        runtime,
        path,
        fn(_response) { prepare_paths(runtime, rest) },
        fn(reason) {
          io.println("cache prepare error " <> reason)
          prepare_paths(runtime, rest)
        },
      )
    }
  }
}

fn evict(
  runtime: Runtime,
  request: Request,
  method: String,
  path: String,
) -> Nil {
  let Runtime(_, cache, Config(version, ..), _, _, _) = runtime
  io.println(int.to_string(version) <> " evict " <> method <> " " <> path)
  cache_delete(cache, request, fn() { Nil }, fn(_reason) { Nil })
}

fn memo_find(
  entries: List(#(String, MemoEntry)),
  key: String,
) -> Option(MemoEntry) {
  case entries {
    [] -> None
    [#(entry_key, entry), ..rest] ->
      case entry_key == key {
        True -> Some(entry)
        False -> memo_find(rest, key)
      }
  }
}

fn memo_replace(
  entries: List(#(String, MemoEntry)),
  key: String,
  entry: MemoEntry,
) -> List(#(String, MemoEntry)) {
  case entries {
    [] -> [#(key, entry)]
    [#(entry_key, old_entry), ..rest] ->
      case entry_key == key {
        True -> [#(entry_key, entry), ..rest]
        False -> [#(entry_key, old_entry), ..memo_replace(rest, key, entry)]
      }
  }
}

fn memo_remove(
  entries: List(#(String, MemoEntry)),
  key: String,
) -> List(#(String, MemoEntry)) {
  case entries {
    [] -> []
    [#(entry_key, entry), ..rest] ->
      case entry_key == key {
        True -> rest
        False -> [#(entry_key, entry), ..memo_remove(rest, key)]
      }
  }
}

fn append_waiter(waiters: List(Waiter), waiter: Waiter) -> List(Waiter) {
  case waiters {
    [] -> [waiter]
    [first, ..rest] -> [first, ..append_waiter(rest, waiter)]
  }
}

fn manifest_values(manifest: List(#(String, String))) -> List(String) {
  case manifest {
    [] -> []
    [#(_, value), ..rest] -> [value, ..manifest_values(rest)]
  }
}

fn find_manifest(
  manifest: List(#(String, String)),
  path: String,
) -> Option(String) {
  case manifest {
    [] -> None
    [#(key, value), ..rest] ->
      case key == path {
        True -> Some(value)
        False -> find_manifest(rest, path)
      }
  }
}

fn remove_value(values: List(String), value: String) -> Option(List(String)) {
  case values {
    [] -> None
    [first, ..rest] ->
      case first == value {
        True -> Some(rest)
        False ->
          case remove_value(rest, value) {
            None -> None
            Some(next) -> Some([first, ..next])
          }
      }
  }
}

fn non_empty(value: Option(String)) -> Option(String) {
  case value {
    Some("") -> None
    value -> value
  }
}
