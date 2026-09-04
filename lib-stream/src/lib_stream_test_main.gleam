import gleam/io
import gleam/list
import lib_stream_sink as sink
import lib_stream_source as source
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

@external(javascript, "./stream_ffi.mjs", "cancel_readable")
fn cancel_readable(
  stream: source.ReadableStream(value),
  done: fn() -> Nil,
) -> Nil

pub fn run(done: fn() -> Nil) {
  let time = lib_time.new_context(lib_time.time())
  let stream = source.source()

  assert source.closed(stream) == False
  collect_strings(source.readable(stream), fn(values) {
    assert values == ["a", "b"]
    assert source.closed(stream) == True

    let output =
      sink.sink(fn(value) {
        assert value == "a" || value == "b"
      })
    write_two_strings(sink.writable(output), "a", "b", fn() {
      io.println("lib-stream sink Web Streams parity passed")

      let immediate_time =
        lib_time.new_context(
          lib_time.custom(fn() { 0.0 }, fn(callback, _milliseconds) {
            callback()
          }),
        )
      let blocked = source.source()
      source.send(immediate_time, blocked, "blocked", fn(result) {
        assert result == Error(source.SendTimedOut)
        io.println("lib-stream source timeout parity passed")
        let failed_close = source.source()
        cancel_readable(source.readable(failed_close), fn() {
          source.close(failed_close, fn(result) {
            assert result == Error(source.CloseFailed)
            assert source.closed(failed_close) == False
            io.println("lib-stream rejected close parity passed")
            io.println("lib-stream source Web Streams parity passed")
            done()
          })
        })
      })
    })
  })

  source.send(time, stream, "a", fn(result) {
    assert result == Ok(Nil)
    source.send(time, stream, "b", fn(result) {
      assert result == Ok(Nil)
      source.close(stream, fn(result) {
        assert result == Ok(Nil)
      })
    })
  })
}

fn collect_strings(
  stream: source.ReadableStream(String),
  done: fn(List(String)) -> Nil,
) -> Nil {
  collect_reader(new_reader(stream), [], done)
}

pub fn main() {
  run(fn() { Nil })
}
