export function new_worker(url) {
  return new globalThis.Worker(url, { type: 'module' })
}

export function client_listen(worker, onResponse) {
  worker.addEventListener('message', (event) => {
    onResponse(event.data)
  })
}

export function client_connect(worker, service, connectionId, input) {
  worker.postMessage(
    {
      clientStream: input,
      connectionId,
      service,
      type: 'WorkerConnectionRequest',
    },
    [input],
  )
}

export function new_server() {
  return globalThis.self
}

export function server_listen(server, onRequest) {
  server.addEventListener('message', (event) => {
    onRequest(event.data)
  })
}

export function message_type(message) {
  return message?.type ?? ''
}

export function message_connection_id(message) {
  return message.connectionId
}

export function message_server_stream(message) {
  return message.serverStream
}

export function message_service(message) {
  return message.service
}

export function message_client_stream(message) {
  return message.clientStream
}

export function server_connect(
  server,
  connectionId,
  output,
  input,
  outputSink,
) {
  server.postMessage(
    {
      connectionId,
      serverStream: output,
      type: 'WorkerConnectionResponse',
    },
    [output],
  )
  input.pipeTo(outputSink).catch((error) => {
    globalThis.setTimeout(() => {
      throw error
    }, 0)
  })
}

export function pipe(readable, writable) {
  readable.pipeTo(writable).catch((error) => {
    globalThis.setTimeout(() => {
      throw error
    }, 0)
  })
}
