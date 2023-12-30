import type * as streamConnection from '@/connection.ts'
import * as streamSink from '@/sink.ts'
import * as streamSource from '@/source.ts'

export type ClientContext = {
  streamClient: {
    worker: Worker
    resolveServerStream: ((
      serverStream: ReadableStream<unknown>
    ) => void)[]
  }
}

export function initClientContext(worker: Worker): ClientContext {
  const resolveServerStream: ((
    serverStream: ReadableStream<unknown>
  ) => void)[] = []

  worker.addEventListener('message', (event) => {
    console.log('worker message', event)
    const connectionResponse =
      event.data as streamConnection.ConnectionResponse
    resolveServerStream[connectionResponse.connectionId]!(
      connectionResponse.serverStream
    )
  })
  console.log('worker', worker)

  return { streamClient: { worker, resolveServerStream } }
}

export function connect(
  ctx: ClientContext,
  service: string,
  onData: (data: unknown) => Promise<void>
): streamSource.Source<unknown> {
  const clientSource = new streamSource.Source()
  const connectionRequest: streamConnection.ConnectionRequest = {
    type: 'ConnectionRequest',
    connectionId: ctx.streamClient.resolveServerStream.length,
    service,
    clientStream: clientSource.readable,
  }
  let resolveServerStream: (serverStream: ReadableStream<unknown>) => void
  const serverStreamPromise = new Promise<ReadableStream<unknown>>(
    (resolve) => (resolveServerStream = resolve)
  )
  ctx.streamClient.resolveServerStream.push(resolveServerStream!)
  ctx.streamClient.worker.postMessage(connectionRequest, [
    connectionRequest.clientStream,
  ])
  const clientSink = new streamSink.Sink(onData)
  void (async () => {
    const serverStream = await serverStreamPromise
    void serverStream.pipeTo(clientSink.writable)
  })()
  return clientSource
}
