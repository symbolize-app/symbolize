import type * as streamConnection from '@/connection.ts'
import * as streamSink from '@/sink.ts'
import * as streamSource from '@/source.ts'

export interface ClientContext {
  readonly streamClient: Client
}

export class Client {
  private readonly mutableResolveServerStream: ((
    serverStream: ReadableStream<unknown>,
  ) => void)[] = []

  private constructor(private readonly worker: Readonly<Worker>) {}

  static init(worker: Readonly<Worker>): Client {
    const client = new Client(worker)

    worker.addEventListener('message', (event) => {
      // eslint-disable-next-line no-console
      console.log('worker message', event)
      const connectionResponse =
        event.data as streamConnection.ConnectionResponse
      const resolve =
        client.mutableResolveServerStream[connectionResponse.connectionId]
      if (!resolve) {
        throw new Error(
          `Invalid connection ID ${connectionResponse.connectionId}`,
        )
      }
      resolve(connectionResponse.serverStream)
    })
    // eslint-disable-next-line no-console
    console.log('worker', worker)

    return client
  }

  connect(
    service: string,
    onData: (data: unknown) => Promise<void>,
  ): streamSource.Source<unknown> {
    const clientSource = streamSource.Source.build()
    const connectionRequest: streamConnection.ConnectionRequest = {
      clientStream: clientSource.readable,
      connectionId: this.mutableResolveServerStream.length,
      service,
      type: 'ConnectionRequest',
    }
    let resolveServerStream: (
      serverStream: ReadableStream<unknown>,
    ) => void
    const serverStreamPromise = new Promise<ReadableStream<unknown>>(
      (resolve) => (resolveServerStream = resolve),
    )
    this.mutableResolveServerStream.push(
      // eslint-disable-next-line @typescript-eslint/no-non-null-assertion -- set by promise constructor
      resolveServerStream!,
    )
    this.worker.postMessage(connectionRequest, [
      connectionRequest.clientStream,
    ])
    const clientSink = streamSink.Sink.build(onData)
    void (async () => {
      const serverStream = await serverStreamPromise
      void serverStream.pipeTo(clientSink.writable)
    })()
    return clientSource
  }
}
