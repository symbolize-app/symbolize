import * as streamSink from '@/sink.ts'
import * as streamSource from '@/source.ts'
import type * as streamWorkerConnection from '@/worker/connection.ts'
import * as collection from '@intertwine/lib-collection'
import type * as time from '@intertwine/lib-time'

declare const self: Readonly<DedicatedWorkerGlobalScope>

export interface WorkerServerContext {
  readonly streamServer: WorkerServer
}

class WorkerServer {
  private readonly connectionRequestSources: collection.Memo<
    string,
    streamSource.Source<streamWorkerConnection.WorkerConnectionRequest>
  > = collection.memo<
    string,
    streamSource.Source<streamWorkerConnection.WorkerConnectionRequest>
  >(() => streamSource.source())

  static build(ctx: time.Context): WorkerServer {
    const server = new WorkerServer()

    self.addEventListener('message', (event) => {
      // eslint-disable-next-line no-console
      console.log('message', event)
      const connectionRequest =
        event.data as streamWorkerConnection.WorkerConnectionRequest
      void server.connectionRequestSources
        .get(connectionRequest.service)
        .send(ctx, connectionRequest)
    })

    return server
  }

  serve(
    service: string,
    onConnect: (serverSource: streamSource.Source<string>) => Promise<{
      onData(data: string): Promise<void>
    }>,
  ): void {
    const connectionRequestStream =
      this.connectionRequestSources.get(service).readable

    const connectionRequestSink = streamSink.sink(
      async (
        connectionRequest: streamWorkerConnection.WorkerConnectionRequest,
      ) => {
        const serverSource = streamSource.source<string>()
        const connectResult = await onConnect(serverSource)
        const connectionResponse: streamWorkerConnection.WorkerConnectionResponse =
          {
            connectionId: connectionRequest.connectionId,
            serverStream: serverSource.readable,
            type: 'WorkerConnectionResponse',
          }
        self.postMessage(connectionResponse, [
          connectionResponse.serverStream,
        ])
        const serverSink = streamSink.sink<string>(async (data) =>
          connectResult.onData(data),
        )
        void connectionRequest.clientStream.pipeTo(serverSink.writable)
      },
    )
    void connectionRequestStream.pipeTo(connectionRequestSink.writable)
  }
}

export type { WorkerServer }

export function workerServer(ctx: time.Context): WorkerServer {
  return WorkerServer.build(ctx)
}
