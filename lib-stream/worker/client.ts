import * as streamSink from '@/sink.ts'
import * as streamSource from '@/source.ts'
import type * as streamWorkerConnection from '@/worker/connection.ts'

export interface WorkerClientContext {
  readonly streamClient: WorkerClient
}

class WorkerClient {
  private readonly mutableResolveServerStream: ((
    serverStream: ReadableStream<unknown>,
  ) => void)[] = []

  private constructor(private readonly worker: Readonly<Worker>) {}

  static build(worker: Readonly<Worker>): WorkerClient {
    const client = new WorkerClient(worker)

    worker.addEventListener('message', (event) => {
      // eslint-disable-next-line no-console
      console.log('worker message', event)
      const connectionResponse =
        event.data as streamWorkerConnection.WorkerConnectionResponse
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
    onData: (data: string) => Promise<void> | void,
  ): streamSource.Source<string> {
    const clientSource = streamSource.source<string>()
    const connectionRequest: streamWorkerConnection.WorkerConnectionRequest =
      {
        clientStream: clientSource.readable,
        connectionId: this.mutableResolveServerStream.length,
        service,
        type: 'WorkerConnectionRequest',
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
    const clientSink = streamSink.sink(onData)
    void (async () => {
      const serverStream = await serverStreamPromise
      void serverStream.pipeTo(clientSink.writable)
    })()
    return clientSource
  }
}

export type { WorkerClient }

export function workerClient(worker: Readonly<Worker>): WorkerClient {
  return WorkerClient.build(worker)
}
