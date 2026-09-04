import gleam/float
import gleam/int
import gleam/option.{type Option, None, Some}
import lib_dataflow as dataflow
import lib_error
import lib_hex
import lib_random
import lib_stream_context as stream_context
import lib_stream_sink as sink
import lib_stream_source as source
import lib_time

pub type Response =
  stream_context.Response

pub type ResponseBody =
  stream_context.ResponseBody

const max_attempts = 40

const min_delay_ms = 100.0

const window_ms = 60_000.0

pub type Context {
  Context(
    retry: lib_error.Context,
    time: lib_time.Context,
    stream: stream_context.Stream,
  )
}

pub opaque type Client {
  Client(
    source: source.Source(String),
    url: String,
    output: sink.Sink(String, String),
    state_context: dataflow.Context,
    connected: dataflow.Mutation(Bool),
  )
}

type ParallelState {
  ParallelState(
    first: Option(Nil),
    second: Option(Nil),
    first_finished: Bool,
    second_finished: Bool,
    error: Option(String),
    settled: Bool,
    done: fn(Result(#(Nil, Nil), String)) -> Nil,
  )
}

@external(javascript, "./stream_ffi.mjs", "response_status")
fn response_status(response: Response) -> Int

@external(javascript, "./stream_ffi.mjs", "response_stream_id")
fn response_stream_id(response: Response) -> String

@external(javascript, "./stream_ffi.mjs", "response_has_body")
fn response_has_body(response: Response) -> Bool

@external(javascript, "./stream_ffi.mjs", "response_body")
fn response_body(response: Response) -> ResponseBody

fn parallel(
  first: lib_error.Async(Nil, String),
  second: lib_error.Async(Nil, String),
  done: fn(Result(#(Nil, Nil), String)) -> Nil,
) -> Nil {
  let context = dataflow.dataflow()
  let state =
    dataflow.state(ParallelState(
      first: None,
      second: None,
      first_finished: False,
      second_finished: False,
      error: None,
      settled: False,
      done: done,
    ))
  case
    call_and_catch(fn() {
      first(fn(result) { accept_parallel(context, state, True, result) })
    })
  {
    Error(reason) -> accept_parallel(context, state, True, Error(reason))
    Ok(_) ->
      case
        call_and_catch(fn() {
          second(fn(result) { accept_parallel(context, state, False, result) })
        })
      {
        Error(reason) -> accept_parallel(context, state, False, Error(reason))
        Ok(_) -> Nil
      }
  }
}

@external(javascript, "./stream_ffi.mjs", "call_and_catch")
fn call_and_catch(callback: fn() -> Nil) -> Result(Nil, String)

@external(javascript, "./stream_ffi.mjs", "queue_microtask")
fn queue_microtask(callback: fn() -> Nil) -> Nil

fn accept_parallel(
  context: dataflow.Context,
  state: dataflow.Mutation(ParallelState),
  is_first: Bool,
  result: Result(Nil, String),
) -> Nil {
  let current =
    dataflow.value(dataflow.to_computation(dataflow.mutation(state)))
  let next = case is_first {
    True ->
      case current.first_finished {
        True -> current
        False -> update_parallel_first(current, result)
      }
    False ->
      case current.second_finished {
        True -> current
        False -> update_parallel_second(current, result)
      }
  }
  let assert Ok(Nil) =
    dataflow.txn(context, fn() { dataflow.set(context, state, next) })
  finish_parallel(context, state)
}

fn update_parallel_first(
  current: ParallelState,
  result: Result(Nil, String),
) -> ParallelState {
  case result {
    Ok(value) ->
      ParallelState(..current, first: Some(value), first_finished: True)
    Error(reason) ->
      ParallelState(
        ..current,
        first_finished: True,
        error: first_parallel_error(current.error, reason),
      )
  }
}

fn update_parallel_second(
  current: ParallelState,
  result: Result(Nil, String),
) -> ParallelState {
  case result {
    Ok(value) ->
      ParallelState(..current, second: Some(value), second_finished: True)
    Error(reason) ->
      ParallelState(
        ..current,
        second_finished: True,
        error: first_parallel_error(current.error, reason),
      )
  }
}

fn first_parallel_error(
  error: Option(String),
  reason: String,
) -> Option(String) {
  case error {
    Some(error) -> Some(error)
    None -> Some(reason)
  }
}

fn finish_parallel(
  context: dataflow.Context,
  state: dataflow.Mutation(ParallelState),
) -> Nil {
  let current =
    dataflow.value(dataflow.to_computation(dataflow.mutation(state)))
  case current.settled {
    True -> Nil
    False ->
      case current.error {
        Some(reason) -> {
          settle_parallel(context, state)
          queue_microtask(fn() { current.done(Error(reason)) })
        }
        None ->
          case current.first, current.second {
            Some(first), Some(second) -> {
              settle_parallel(context, state)
              queue_microtask(fn() { current.done(Ok(#(first, second))) })
            }
            _, _ -> Nil
          }
      }
  }
}

fn settle_parallel(
  context: dataflow.Context,
  state: dataflow.Mutation(ParallelState),
) -> Nil {
  let current =
    dataflow.value(dataflow.to_computation(dataflow.mutation(state)))
  let _ =
    dataflow.txn(context, fn() {
      dataflow.set(context, state, ParallelState(..current, settled: True))
    })
  Nil
}

@external(javascript, "./stream_ffi.mjs", "console_log")
fn console_log(message: String) -> Nil

@external(javascript, "./stream_ffi.mjs", "console_error")
fn console_error(message: String) -> Nil

fn console_retry_error(
  kind: String,
  attempt: Int,
  delay_ms: Float,
  reason: String,
) -> Nil {
  console_error(
    "Error connnecting "
    <> kind
    <> " stream (attempt "
    <> int.to_string(attempt)
    <> ", next delay "
    <> float.to_string(delay_ms)
    <> " ms) "
    <> reason,
  )
}

pub fn context() -> Context {
  let time = lib_time.new_context(lib_time.time())
  context_with(lib_random.random(), time)
}

pub fn context_with(
  random: lib_random.Random,
  time: lib_time.Context,
) -> Context {
  context_with_stream(random, time, stream_context.browser())
}

pub fn context_with_stream(
  random: lib_random.Random,
  time: lib_time.Context,
  stream: stream_context.Stream,
) -> Context {
  Context(
    retry: lib_error.new_context(random, time),
    time: time,
    stream: stream,
  )
}

pub fn context_stream(context: Context) -> stream_context.Stream {
  context.stream
}

pub fn time(context: Context) -> lib_time.Context {
  context.time
}

pub fn browser_url(path: String) -> String {
  stream_context.browser_url(path)
}

pub fn client(
  context: Context,
  url: String,
  on_data: fn(String) -> Nil,
) -> Client {
  let input = source.source()
  let output = sink.sink(on_data)
  let client = new_client(input, url, output)
  connect(client, context)
  client
}

pub fn client_async(
  context: Context,
  url: String,
  on_data: sink.AsyncHandler(String, String),
) -> Client {
  let input = source.source()
  let output = sink.async_sink(on_data)
  let client = new_client(input, url, output)
  connect(client, context)
  client
}

fn new_client(
  input: source.Source(String),
  url: String,
  output: sink.Sink(String, String),
) -> Client {
  Client(
    source: input,
    url: url,
    output: output,
    state_context: dataflow.dataflow(),
    connected: dataflow.state(False),
  )
}

pub fn connect(client: Client, context: Context) -> Nil {
  let Client(input, url, output, state_context, connected) = client
  let is_connected =
    dataflow.value(dataflow.to_computation(dataflow.mutation(connected)))
  case is_connected {
    True -> Nil
    False -> {
      let assert Ok(Nil) =
        dataflow.txn(state_context, fn() {
          dataflow.set(state_context, connected, True)
        })
      connect_response(context, url, input, output, state_context, connected)
    }
  }
}

fn connect_response(
  context: Context,
  url: String,
  input: source.Source(String),
  output: sink.Sink(String, String),
  state_context: dataflow.Context,
  connected: dataflow.Mutation(Bool),
) -> Nil {
  let operation = fn(done) {
    run_one_response_stream(context, url, input, output, done)
  }
  let config =
    lib_error.RetryConfig(
      max_attempts: max_attempts,
      min_delay_ms: min_delay_ms,
      on_error: option.Some(fn(reason, attempt, delay_ms) {
        console_retry_error("response", attempt, delay_ms, reason)
      }),
      signal: option.None,
      window_ms: window_ms,
    )
  lib_error.retry(context.retry, operation, config)(fn(result) {
    let assert Ok(Nil) =
      dataflow.txn(state_context, fn() {
        dataflow.set(state_context, connected, False)
      })
    console_log("HTTP response stream retry done")
    case result {
      Ok(Nil) -> Nil
      Error(error) -> panic as error
    }
  })
  Nil
}

pub fn source(client: Client) -> source.Source(String) {
  let Client(input, _, _, _, _) = client
  input
}

fn run_one_response_stream(
  context: Context,
  url: String,
  input: source.Source(String),
  output: sink.Sink(String, String),
  done: fn(Result(Nil, String)) -> Nil,
) -> Nil {
  case source.closed(input) {
    True -> done(Ok(Nil))
    False ->
      stream_context.fetch_response(
        context.stream,
        url,
        fn(response) {
          case response_status(response) {
            200 -> start_streams(context, response, url, input, output, done)
            status -> done(Error("bad response " <> int.to_string(status)))
          }
        },
        fn(error) { done(Error(error)) },
      )
  }
}

fn start_streams(
  context: Context,
  response: Response,
  url: String,
  input: source.Source(String),
  output: sink.Sink(String, String),
  done: fn(Result(Nil, String)) -> Nil,
) -> Nil {
  let response_stream_id = response_stream_id(response)
  case response_stream_id == "" {
    True -> done(Error("missing response stream ID"))
    False -> {
      let response_stream_id =
        lib_hex.uint8_array_to_hex(lib_hex.uint8_array_from_hex(
          response_stream_id,
        ))
      console_log("HTTP response stream " <> response_stream_id <> " started")
      case response_has_body(response) {
        False -> done(Error("missing response body"))
        True -> {
          let controller = lib_time.new_abort_controller()
          let signal = lib_time.abort_controller_signal(controller)
          let request = fn(request_done) {
            run_retry_request_streams(
              context,
              url,
              response_stream_id,
              input,
              controller,
              signal,
              request_done,
            )
          }
          let response_stream = fn(response_done) {
            stream_context.pipe_response(
              response_body(response),
              sink.writable(output),
              controller,
              fn() {
                lib_time.abort_controller_abort(controller)
                response_done(Ok(Nil))
              },
              fn(error) {
                lib_time.abort_controller_abort(controller)
                response_done(Error(error))
              },
            )
          }
          parallel(request, response_stream, fn(result) {
            lib_time.abort_controller_abort(controller)
            case result {
              Error(error) -> done(Error(error))
              Ok(_) ->
                case source.closed(input) {
                  True -> {
                    console_log(
                      "HTTP response stream " <> response_stream_id <> " done",
                    )
                    done(Ok(Nil))
                  }
                  False ->
                    done(Error(
                      "Unexpected HTTP response stream "
                      <> response_stream_id
                      <> " done",
                    ))
                }
            }
          })
        }
      }
    }
  }
}

fn run_retry_request_streams(
  context: Context,
  url: String,
  response_stream_id: String,
  input: source.Source(String),
  controller: lib_time.AbortController,
  signal: lib_time.AbortSignal,
  done: fn(Result(Nil, String)) -> Nil,
) -> Nil {
  case lib_time.signal_aborted(signal) {
    True -> done(Ok(Nil))
    False -> {
      let operation = fn(operation_done) {
        run_one_request_stream(
          context,
          url,
          response_stream_id,
          input,
          signal,
          operation_done,
        )
      }
      let config =
        lib_error.RetryConfig(
          max_attempts: max_attempts,
          min_delay_ms: min_delay_ms,
          on_error: option.Some(fn(reason, attempt, delay_ms) {
            console_retry_error("request", attempt, delay_ms, reason)
          }),
          signal: option.Some(signal),
          window_ms: window_ms,
        )
      lib_error.retry(context.retry, operation, config)(fn(result) {
        lib_time.abort_controller_abort(controller)
        console_log("HTTP request stream retry done")
        done(result)
      })
    }
  }
}

fn run_one_request_stream(
  context: Context,
  url: String,
  response_stream_id: String,
  input: source.Source(String),
  signal: lib_time.AbortSignal,
  done: fn(Result(Nil, String)) -> Nil,
) -> Nil {
  case source.closed(input) {
    True -> done(Ok(Nil))
    False -> {
      let request_url =
        stream_context.with_response_stream_id(url, response_stream_id)
      let start_ms = lib_time.performance_now(context.time)
      console_log(
        "HTTP request stream " <> response_stream_id <> " starting...",
      )
      stream_context.fetch_request(
        context.stream,
        request_url,
        source.readable(input),
        signal,
        fn(response) {
          let result = case response_status(response) {
            200 -> Ok(Nil)
            status -> Error("bad response " <> int.to_string(status))
          }
          finish_request_stream(
            context,
            input,
            response_stream_id,
            start_ms,
            result,
            False,
            done,
          )
        },
        fn(error, aborted) {
          finish_request_stream(
            context,
            input,
            response_stream_id,
            start_ms,
            Error(error),
            aborted,
            done,
          )
        },
      )
    }
  }
}

fn finish_request_stream(
  context: Context,
  input: source.Source(String),
  response_stream_id: String,
  start_ms: Float,
  result: Result(Nil, String),
  aborted: Bool,
  done: fn(Result(Nil, String)) -> Nil,
) -> Nil {
  let duration = lib_time.performance_now(context.time) -. start_ms
  let result = case source.closed(input) {
    True ->
      case aborted {
        True -> Ok(Nil)
        False -> result
      }
    False ->
      Error("Unexpected HTTP request stream " <> response_stream_id <> " done")
  }
  case result {
    Ok(_) -> {
      console_log("HTTP request stream " <> response_stream_id <> " done")
      done(result)
    }
    Error(_) ->
      case duration >. window_ms /. 2.0 {
        True -> {
          console_log(
            "Delayed HTTP request stream " <> response_stream_id <> " done",
          )
          done(Ok(Nil))
        }
        False -> done(result)
      }
  }
}
