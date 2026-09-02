import gleam/io
import gleam/list
import gleam/option.{type Option}
import lib_dataflow as dataflow
import lib_random
import lib_stream_context as stream_context
import lib_stream_http as http
import lib_stream_source as source
import lib_time

@external(javascript, "./stream_ffi.mjs", "new_response")
fn new_response(
  body: source.ReadableStream(BitArray),
  response_stream_id: String,
) -> stream_context.Response

@external(javascript, "./stream_ffi.mjs", "empty_response")
fn empty_response() -> stream_context.Response

@external(javascript, "./stream_ffi.mjs", "decode_request_body")
fn decode_request_body(
  body: source.ReadableStream(BitArray),
  on_chunk: fn(String) -> Nil,
  on_success: fn() -> Nil,
  on_failure: fn(String) -> Nil,
) -> Nil

fn custom_response(
  time: lib_time.Context,
  on_success: fn(stream_context.Response) -> Nil,
  on_failure: fn(String) -> Nil,
) -> Nil {
  let body = source.source()
  source.send(time, body, <<99>>, fn(result) {
    case result {
      Error(_) -> on_failure("failed to send response chunk")
      Ok(Nil) -> Nil
    }
  })
  source.send(time, body, <<100>>, fn(result) {
    case result {
      Error(_) -> on_failure("failed to send response chunk")
      Ok(Nil) -> Nil
    }
  })
  source.close(body, fn(result) {
    case result {
      Error(_) -> on_failure("failed to close response stream")
      Ok(Nil) -> Nil
    }
  })
  on_success(new_response(source.readable(body), "abcd"))
}

fn custom_request(
  body: source.ReadableStream(BitArray),
  on_chunk: fn(String) -> Nil,
  on_success: fn(stream_context.Response) -> Nil,
  on_failure: fn(String) -> Nil,
) -> Nil {
  decode_request_body(
    body,
    on_chunk,
    fn() { on_success(empty_response()) },
    on_failure,
  )
}

type History(value) {
  History(context: dataflow.Context, state: dataflow.Mutation(List(value)))
}

type TestState {
  TestState(history: History(String), writes: History(String))
}

fn new_history() -> History(value) {
  History(dataflow.dataflow(), dataflow.state([]))
}

fn new_test_state() -> TestState {
  TestState(history: new_history(), writes: new_history())
}

fn push_history(history: History(value), value: value) -> Nil {
  let History(context, state) = history
  let current =
    dataflow.value(dataflow.to_computation(dataflow.mutation(state)))
  let assert Ok(Nil) =
    dataflow.txn(context, fn() {
      dataflow.set(context, state, [value, ..current])
    })
  Nil
}

fn history_values(history: History(value)) -> List(value) {
  let History(_, state) = history
  let values = dataflow.value(dataflow.to_computation(dataflow.mutation(state)))
  list.reverse(values)
}

fn custom_fetch(
  time: lib_time.Context,
  state: TestState,
  url: String,
  body: stream_context.RequestBody,
  _signal: Option(lib_time.AbortSignal),
  on_success: fn(stream_context.Response) -> Nil,
  on_failure: fn(String) -> Nil,
) -> Nil {
  let TestState(history, writes) = state
  push_history(history, url)
  case history_values(history) {
    ["https://example.org/stream"] ->
      custom_response(time, on_success, on_failure)
    [_, ..] ->
      case url == "https://example.org/stream?response_stream_id=abcd" {
        True ->
          case body {
            stream_context.StreamBody(body) ->
              custom_request(
                body,
                fn(chunk) { push_history(writes, chunk) },
                on_success,
                on_failure,
              )
            stream_context.Empty -> on_failure("missing request stream")
          }
        False -> on_failure("unexpected request URL " <> url)
      }
    _ -> on_failure("unexpected request URL " <> url)
  }
}

pub fn main() {
  let state = new_test_state()
  let time = lib_time.new_context(lib_time.time())
  let stream =
    stream_context.custom(fn(url, body, signal, on_success, on_failure) {
      custom_fetch(time, state, url, body, signal, on_success, on_failure)
    })
  let context = http.context_with_stream(lib_random.random(), time, stream)
  let client =
    http.client_async(context, "https://example.org/stream", fn(data) {
      fn(done) {
        assert data == "c" || data == "d"
        case data == "d" {
          True -> {
            let TestState(history, writes) = state
            assert history_values(history)
              == [
                "https://example.org/stream",
                "https://example.org/stream?response_stream_id=abcd",
              ]
            assert history_values(writes) == ["a", "b"]
            io.println("lib-stream custom Fetch context parity passed")
          }
          False -> Nil
        }
        done(Ok(Nil))
      }
    })
  http.connect(client, context)
  let input = http.source(client)
  source.send(time, input, "a", fn(result) {
    assert result == Ok(Nil)
    source.send(time, input, "b", fn(result) {
      assert result == Ok(Nil)
      source.close(input, fn(result) {
        assert result == Ok(Nil)
      })
    })
  })
}
