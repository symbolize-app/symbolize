import gleam/option.{type Option, None, Some}
import lib_stream_sink as sink
import lib_stream_source as source
import lib_time

/// The response and body are JavaScript Web API values. They stay opaque to
/// Gleam and are only inspected by the Web API boundary in this package.
pub type Response

pub type ResponseBody

pub type BrowserFetch

pub type RequestBody {
  Empty
  StreamBody(source.ReadableStream(BitArray))
}

pub type Context {
  Context(stream: Stream)
}

pub opaque type Stream {
  StreamHandle(fetch: Fetch)
}

type Fetch {
  Browser(BrowserFetch)
  Custom(
    fn(
      String,
      RequestBody,
      Option(lib_time.AbortSignal),
      fn(Response) -> Nil,
      fn(String) -> Nil,
    ) -> Nil,
  )
}

@external(javascript, "./stream_ffi.mjs", "new_fetch")
fn new_fetch() -> BrowserFetch

@external(javascript, "./stream_ffi.mjs", "browser_url")
fn browser_url_ffi(path: String) -> String

@external(javascript, "./stream_ffi.mjs", "fetch_response")
fn browser_fetch_response(
  fetch: BrowserFetch,
  url: String,
  on_success: fn(Response) -> Nil,
  on_failure: fn(String) -> Nil,
) -> Nil

@external(javascript, "./stream_ffi.mjs", "fetch_request")
fn browser_fetch_request(
  fetch: BrowserFetch,
  url: String,
  input: source.ReadableStream(String),
  signal: lib_time.AbortSignal,
  on_success: fn(Response) -> Nil,
  on_failure: fn(String, Bool) -> Nil,
) -> Nil

@external(javascript, "./stream_ffi.mjs", "encode_request_body")
fn encode_request_body(
  input: source.ReadableStream(String),
) -> source.ReadableStream(BitArray)

@external(javascript, "./stream_ffi.mjs", "with_response_stream_id")
fn browser_with_response_stream_id(
  url: String,
  response_stream_id: String,
) -> String

@external(javascript, "./stream_ffi.mjs", "pipe_response")
fn browser_pipe_response(
  body: ResponseBody,
  output: sink.WritableStream(String),
  controller: lib_time.AbortController,
  on_success: fn() -> Nil,
  on_failure: fn(String, Bool) -> Nil,
) -> Nil

pub fn context() -> Context {
  Context(stream: browser())
}

pub fn browser() -> Stream {
  StreamHandle(Browser(new_fetch()))
}

pub fn custom(
  fetch: fn(
    String,
    RequestBody,
    Option(lib_time.AbortSignal),
    fn(Response) -> Nil,
    fn(String) -> Nil,
  ) -> Nil,
) -> Stream {
  StreamHandle(Custom(fetch))
}

pub fn context_with(stream: Stream) -> Context {
  Context(stream: stream)
}

pub fn stream(context: Context) -> Stream {
  context.stream
}

pub fn browser_url(path: String) -> String {
  browser_url_ffi(path)
}

pub fn fetch_response(
  stream: Stream,
  url: String,
  on_success: fn(Response) -> Nil,
  on_failure: fn(String) -> Nil,
) -> Nil {
  case stream {
    StreamHandle(Browser(fetch)) ->
      browser_fetch_response(fetch, url, on_success, on_failure)
    StreamHandle(Custom(fetch)) ->
      fetch(url, Empty, None, on_success, on_failure)
  }
}

pub fn fetch_request(
  stream: Stream,
  url: String,
  input: source.ReadableStream(String),
  signal: lib_time.AbortSignal,
  on_success: fn(Response) -> Nil,
  on_failure: fn(String, Bool) -> Nil,
) -> Nil {
  case stream {
    StreamHandle(Browser(fetch)) ->
      browser_fetch_request(fetch, url, input, signal, on_success, on_failure)
    StreamHandle(Custom(fetch)) ->
      fetch(
        url,
        StreamBody(encode_request_body(input)),
        Some(signal),
        on_success,
        fn(error) { on_failure(error, False) },
      )
  }
}

pub fn with_response_stream_id(
  url: String,
  response_stream_id: String,
) -> String {
  browser_with_response_stream_id(url, response_stream_id)
}

pub fn pipe_response(
  body: ResponseBody,
  output: sink.WritableStream(String),
  controller: lib_time.AbortController,
  on_success: fn() -> Nil,
  on_failure: fn(String) -> Nil,
) -> Nil {
  browser_pipe_response(
    body,
    output,
    controller,
    on_success,
    fn(error, aborted) {
      case aborted {
        True -> on_success()
        False -> on_failure(error)
      }
    },
  )
}
