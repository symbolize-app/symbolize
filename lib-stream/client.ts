import type * as streamConnection from '@/connection.ts'
import * as streamSink from '@/sink.ts'
import * as streamSource from '@/source.ts'

export interface ClientContext {
  readonly streamClient: {
    readonly mutableResolveServerStream: ((
      serverStream: ReadableStream<unknown>
    ) => void)[]
    readonly worker: Readonly<Worker>
  }
}

export function initClientContext(
  worker: Readonly<Worker>
): ClientContext {
  const resolveServerStream: ((
    serverStream: ReadableStream<unknown>
  ) => void)[] = []

  worker.addEventListener('message', (event) => {
    // eslint-disable-next-line no-console
    console.log('worker message', event)
    const connectionResponse =
      event.data as streamConnection.ConnectionResponse
    const resolve = resolveServerStream[connectionResponse.connectionId]
    if (!resolve) {
      throw new Error(
        `Invalid connection ID ${connectionResponse.connectionId}`
      )
    }
    resolve(connectionResponse.serverStream)
  })
  // eslint-disable-next-line no-console
  console.log('worker', worker)

  return {
    streamClient: {
      mutableResolveServerStream: resolveServerStream,
      worker,
    },
  }
}

export function connect(
  ctx: ClientContext,
  service: string,
  onData: (data: unknown) => Promise<void>
): streamSource.Source<unknown> {
  const clientSource = new streamSource.Source()
  const connectionRequest: streamConnection.ConnectionRequest = {
    clientStream: clientSource.readable,
    connectionId: ctx.streamClient.mutableResolveServerStream.length,
    service,
    type: 'ConnectionRequest',
  }
  let resolveServerStream: (serverStream: ReadableStream<unknown>) => void
  const serverStreamPromise = new Promise<ReadableStream<unknown>>(
    (resolve) => (resolveServerStream = resolve)
  )
  ctx.streamClient.mutableResolveServerStream.push(
    // eslint-disable-next-line @typescript-eslint/no-non-null-assertion -- set by promise constructor
    resolveServerStream!
  )
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
