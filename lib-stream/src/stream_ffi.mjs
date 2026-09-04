import { Result$Error, Result$Error$0, Result$Ok } from '../prelude.mjs'

export function new_source(highWaterMark) {
  const transform = new TransformStream(undefined, { highWaterMark })
  const writer = transform.writable.getWriter()
  return [transform.readable, writer]
}

export function watch_source_closed(writer, onClosed) {
  writer.closed.then(onClosed).catch(console.error)
}

export function cancel_readable(readable, done) {
  readable.cancel().then(done, done)
}

export function new_sink(highWaterMark, onData) {
  return new WritableStream(
    {
      write(chunk) {
        return new Promise((resolve, reject) => {
          try {
            onData(chunk, resolve, reject)
          } catch (error) {
            reject(error)
          }
        })
      },
    },
    { highWaterMark },
  )
}

export function writer_write(writer, value, onSuccess, onFailure) {
  try {
    Promise.resolve(writer.write(foreign_value(value))).then(
      onSuccess,
      onFailure,
    )
  } catch (_error) {
    onFailure()
  }
}

function foreign_value(value) {
  return value?.rawBuffer ?? value
}

export function new_response(body, responseStreamId) {
  return new Response(body, {
    headers: { 'response-stream-id': responseStreamId },
  })
}

export function empty_response() {
  return new Response()
}

export function decode_request_body(body, onChunk, onSuccess, onFailure) {
  try {
    body
      .pipeThrough(new TextDecoderStream())
      .pipeTo(
        new WritableStream({
          write: onChunk,
        }),
      )
      .then(onSuccess, (error) => onFailure(String(error)))
  } catch (error) {
    onFailure(String(error))
  }
}

export function queue_microtask(callback) {
  globalThis.queueMicrotask(callback)
}

export function call_and_catch(callback) {
  try {
    callback()
    return Result$Ok(undefined)
  } catch (error) {
    return Result$Error(Result$Error$0(String(error)))
  }
}

export function new_fetch() {
  return globalThis.fetch.bind(globalThis)
}

export function console_log(message) {
  console.log(message)
}

export function console_error(message) {
  console.error(message)
}

export function browser_url(path) {
  return new URL(path, globalThis.location.origin).toString()
}

export function fetch_response(fetch, url, onSuccess, onFailure) {
  fetch(url, { method: 'POST' }).then(
    (response) => onSuccess(response),
    (error) => onFailure(String(error)),
  )
}

export function fetch_request(
  fetch,
  url,
  input,
  signal,
  onSuccess,
  onFailure,
) {
  let request
  try {
    request = fetch(url, {
      body: input.pipeThrough(new TextEncoderStream(), {
        preventCancel: true,
      }),
      duplex: 'half',
      method: 'POST',
      signal,
    })
  } catch (error) {
    onFailure(String(error), isAbortError(error))
    return
  }

  Promise.resolve(request).then(
    (response) => onSuccess(response),
    (error) => onFailure(String(error), isAbortError(error)),
  )
}

export function encode_request_body(input) {
  return input.pipeThrough(new TextEncoderStream())
}

function isAbortError(error) {
  return error instanceof DOMException && error.name === 'AbortError'
}

export function with_response_stream_id(url, responseStreamId) {
  const requestUrl = new URL(url)
  requestUrl.searchParams.set('response_stream_id', responseStreamId)
  return requestUrl.toString()
}

export function response_status(response) {
  return response.status
}

export function response_stream_id(response) {
  return response.headers.get('response-stream-id') ?? ''
}

export function response_has_body(response) {
  return response.body !== null
}

export function response_body(response) {
  return response.body
}

export function pipe_response(
  body,
  output,
  controller,
  onSuccess,
  onFailure,
) {
  body
    .pipeThrough(new TextDecoderStream())
    .pipeTo(output, {
      preventAbort: true,
      preventCancel: true,
      preventClose: true,
      signal: controller.signal,
    })
    .then(onSuccess, (error) =>
      onFailure(String(error), isAbortError(error)),
    )
}

export function writer_close(writer, onSuccess, onFailure) {
  try {
    Promise.resolve(writer.close()).then(onSuccess, onFailure)
  } catch (_error) {
    onFailure()
  }
}

export function new_reader(stream) {
  return stream.getReader()
}

export function read(reader, onValue, onDone) {
  reader.read().then((result) => {
    if (result.done) onDone()
    else onValue(result.value)
  })
}

export function schedule(callback) {
  globalThis.setTimeout(callback, 0)
}

export function new_writer(stream) {
  return stream.getWriter()
}

export function write(writer, value, done) {
  writer.write(value).then(done)
}

export function close(writer, done) {
  writer.close().then(done)
}
