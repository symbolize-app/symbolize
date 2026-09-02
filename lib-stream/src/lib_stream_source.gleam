import lib_dataflow as dataflow
import lib_time

const high_water_mark = 16

const timeout_ms = 1000.0

pub type ReadableStream(value)

pub type Writer(value)

pub opaque type Source(value) {
  Source(
    readable: ReadableStream(value),
    writer: Writer(value),
    context: dataflow.Context,
    closed: dataflow.Mutation(Bool),
  )
}

pub type SendError {
  SendTimedOut
  SendFailed
}

pub type CloseError {
  CloseFailed
}

@external(javascript, "./stream_ffi.mjs", "new_source")
fn new_source(high_water_mark: Int) -> #(ReadableStream(value), Writer(value))

@external(javascript, "./stream_ffi.mjs", "watch_source_closed")
fn watch_source_closed(writer: Writer(value), on_closed: fn() -> Nil) -> Nil

@external(javascript, "./stream_ffi.mjs", "writer_write")
fn writer_write(
  writer: Writer(value),
  value: value,
  on_success: fn() -> Nil,
  on_failure: fn() -> Nil,
) -> Nil

@external(javascript, "./stream_ffi.mjs", "writer_close")
fn writer_close(
  writer: Writer(value),
  on_success: fn() -> Nil,
  on_failure: fn() -> Nil,
) -> Nil

pub fn source() -> Source(value) {
  let #(readable, writer) = new_source(high_water_mark)
  let context = dataflow.dataflow()
  let closed = dataflow.state(False)
  watch_source_closed(writer, fn() {
    let assert Ok(Nil) =
      dataflow.txn(context, fn() { dataflow.set(context, closed, True) })
    Nil
  })
  Source(readable: readable, writer: writer, context: context, closed: closed)
}

pub fn readable(source: Source(value)) -> ReadableStream(value) {
  source.readable
}

pub fn closed(source: Source(value)) -> Bool {
  let Source(_, _, _, closed) = source
  dataflow.value(dataflow.to_computation(dataflow.mutation(closed)))
}

pub fn send(
  context: lib_time.Context,
  source: Source(value),
  value: value,
  done: fn(Result(Nil, SendError)) -> Nil,
) -> Nil {
  let state = dataflow.state(False)
  let finish = fn(result: Result(Nil, SendError)) {
    let settled =
      dataflow.value(dataflow.to_computation(dataflow.mutation(state)))
    case settled {
      True -> Nil
      False -> {
        let assert Ok(Nil) =
          dataflow.txn(source.context, fn() {
            dataflow.set(source.context, state, True)
          })
        done(result)
      }
    }
  }
  writer_write(source.writer, value, fn() { finish(Ok(Nil)) }, fn() {
    finish(Error(SendFailed))
  })
  lib_time.set_timeout(
    context,
    fn() { finish(Error(SendTimedOut)) },
    timeout_ms,
  )
}

pub fn close(
  source: Source(value),
  done: fn(Result(Nil, CloseError)) -> Nil,
) -> Nil {
  writer_close(source.writer, fn() { done(Ok(Nil)) }, fn() {
    done(Error(CloseFailed))
  })
}
