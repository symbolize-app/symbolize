import gleam/io
import gleam/list
import lib_stream_http as http
import lib_stream_sink as sink
import lib_stream_source as source
import lib_stream_worker
import lib_time

type Reader

type Writer

@external(javascript, "./stream_ffi.mjs", "new_reader")
fn new_reader(stream: source.ReadableStream(String)) -> Reader

@external(javascript, "./stream_ffi.mjs", "read")
fn read(
  reader: Reader,
  on_value: fn(String) -> Nil,
  on_done: fn() -> Nil,
) -> Nil

@external(javascript, "./stream_ffi.mjs", "schedule")
fn schedule(callback: fn() -> Nil) -> Nil

fn collect_reader(
  reader: Reader,
  values: List(String),
  done: fn(List(String)) -> Nil,
) -> Nil {
  read(
    reader,
    fn(value) { collect_reader(reader, [value, ..values], done) },
    fn() { schedule(fn() { done(list.reverse(values)) }) },
  )
}

fn collect_strings(
  stream: source.ReadableStream(String),
  done: fn(List(String)) -> Nil,
) -> Nil {
  collect_reader(new_reader(stream), [], done)
}

@external(javascript, "./stream_ffi.mjs", "new_writer")
fn new_writer(stream: sink.WritableStream(String)) -> Writer

@external(javascript, "./stream_ffi.mjs", "write")
fn write(writer: Writer, value: String, done: fn() -> Nil) -> Nil

@external(javascript, "./stream_ffi.mjs", "close")
fn close(writer: Writer, done: fn() -> Nil) -> Nil

fn write_two_strings(
  stream: sink.WritableStream(String),
  first: String,
  second: String,
  done: fn() -> Nil,
) -> Nil {
  let writer = new_writer(stream)
  write(writer, first, fn() {
    write(writer, second, fn() { close(writer, done) })
  })
}

pub fn main(url: String, worker_url: String) {
  let time = lib_time.new_context(lib_time.time())
  let stream = source.source()
  collect_strings(source.readable(stream), fn(values) {
    let assert ["browser-a", "browser-b"] = values
    io.println("lib-stream Chromium Web Streams FFI passed")
  })
  source.send(time, stream, "browser-a", fn(result) {
    let assert Ok(Nil) = result
    source.send(time, stream, "browser-b", fn(result) {
      let assert Ok(Nil) = result
      source.close(stream, fn(result) {
        let assert Ok(Nil) = result
        Nil
      })
    })
  })

  let output =
    sink.sink(fn(value) {
      assert value == "browser-a" || value == "browser-b"
      Nil
    })
  write_two_strings(sink.writable(output), "browser-a", "browser-b", fn() {
    io.println("lib-stream Chromium Web WritableStream FFI passed")
  })

  let immediate_time =
    lib_time.new_context(
      lib_time.custom(fn() { 0.0 }, fn(callback, _milliseconds) { callback() }),
    )
  let blocked = source.source()
  source.send(immediate_time, blocked, "blocked", fn(result) {
    let assert Error(source.SendTimedOut) = result
    Nil
  })

  let client =
    http.client(http.context(), url, fn(data) {
      assert data == "c"
      io.println("lib-stream Chromium HTTP Fetch parity passed")
    })
  let input = http.source(client)
  source.send(time, input, "a", fn(result) {
    let assert Ok(Nil) = result
    source.send(time, input, "b", fn(result) {
      let assert Ok(Nil) = result
      source.close(input, fn(result) {
        let assert Ok(Nil) = result
        Nil
      })
    })
  })

  let worker =
    lib_stream_worker.new_worker(worker_url)
    |> lib_stream_worker.client
  let worker_input =
    lib_stream_worker.connect(worker, "echo", fn(data) {
      assert data == "echo-a" || data == "echo-b"
      case data == "echo-b" {
        True -> io.println("lib-stream Chromium Worker FFI passed")
        False -> Nil
      }
    })
  source.send(time, worker_input, "a", fn(result) {
    assert result == Ok(Nil)
    source.send(time, worker_input, "b", fn(result) {
      assert result == Ok(Nil)
      source.close(worker_input, fn(result) {
        assert result == Ok(Nil)
      })
    })
  })

  let async_connect_input =
    lib_stream_worker.connect(worker, "async-connect", fn(data) {
      assert data == "connected"
      io.println("lib-stream Chromium Worker async connect passed")
    })
  source.send(time, async_connect_input, "request", fn(result) {
    assert result == Ok(Nil)
    source.close(async_connect_input, fn(close_result) {
      assert close_result == Ok(Nil)
    })
  })
}
