import type * as streamConnection from '@/connection.ts'
import * as streamSink from '@/sink.ts'
import * as streamSource from '@/source.ts'
import * as collection from '@intertwine/lib-collection'
import type * as time from '@intertwine/lib-time'

declare const self: Readonly<DedicatedWorkerGlobalScope>

export interface ServerContext {
  readonly streamServer: Server
}

export class Server {
  private readonly connectionRequestSources: collection.Memo<
    string,
    streamSource.Source<streamConnection.ConnectionRequest>
  > = new collection.Memo<
    string,
    streamSource.Source<streamConnection.ConnectionRequest>
  >(() => streamSource.Source.build())

  private constructor() {
    // Private
  }

  static init(ctx: time.Context): Server {
    const server = new Server()

    self.addEventListener('message', (event) => {
      // eslint-disable-next-line no-console
      console.log('message', event)
      const connectionRequest =
        event.data as streamConnection.ConnectionRequest
      void server.connectionRequestSources
        .get(connectionRequest.service)
        .send(ctx, connectionRequest)
    })

    return server
  }

  serve(
    service: string,
    onConnect: (serverSource: streamSource.Source<unknown>) => Promise<{
      onData(data: unknown): Promise<void>
    }>,
  ): void {
    const connectionRequestStream =
      this.connectionRequestSources.get(service).readable

    const connectionRequestSink = streamSink.Sink.build(
      async (connectionRequest: streamConnection.ConnectionRequest) => {
        const serverSource = streamSource.Source.build()
        const connectResult = await onConnect(serverSource)
        const connectionResponse: streamConnection.ConnectionResponse = {
          connectionId: connectionRequest.connectionId,
          serverStream: serverSource.readable,
          type: 'ConnectionResponse',
        }
        self.postMessage(connectionResponse, [
          connectionResponse.serverStream,
        ])
        const serverSink = streamSink.Sink.build(async (data) =>
          connectResult.onData(data),
        )
        void connectionRequest.clientStream.pipeTo(serverSink.writable)
        return Promise.resolve()
      },
    )
    void connectionRequestStream.pipeTo(connectionRequestSink.writable)
  }
}
