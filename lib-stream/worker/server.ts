import * as streamSink from '@/sink.ts'
import * as streamSource from '@/source.ts'
import type * as streamWorkerConnection from '@/worker/connection.ts'
import * as collection from '@intertwine/lib-collection'
import type * as time from '@intertwine/lib-time'

declare const self: Readonly<DedicatedWorkerGlobalScope>

export interface WorkerServerContext {
  readonly streamServer: WorkerServer
}

export class WorkerServer {
  private readonly connectionRequestSources: collection.Memo<
    string,
    streamSource.Source<streamWorkerConnection.WorkerConnectionRequest>
  > = new collection.Memo<
    string,
    streamSource.Source<streamWorkerConnection.WorkerConnectionRequest>
  >(() => streamSource.Source.build())

  private constructor() {}

  static init(ctx: time.Context): WorkerServer {
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

    const connectionRequestSink = streamSink.Sink.build(
      async (
        connectionRequest: streamWorkerConnection.WorkerConnectionRequest,
      ) => {
        const serverSource = streamSource.Source.build<string>()
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
        const serverSink = streamSink.Sink.build<string>(async (data) =>
          connectResult.onData(data),
        )
        void connectionRequest.clientStream.pipeTo(serverSink.writable)
      },
    )
    void connectionRequestStream.pipeTo(connectionRequestSink.writable)
  }
}
