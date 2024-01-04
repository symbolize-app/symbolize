import type * as streamConnection from '@/connection.ts'
import * as streamSink from '@/sink.ts'
import * as streamSource from '@/source.ts'
import * as collection from '@intertwine/lib-collection'
import type * as time from '@intertwine/lib-time'

declare const self: DedicatedWorkerGlobalScope

export type ServerContext = {
  streamServer: {
    connectionRequestSources: collection.Memo<
      string,
      streamSource.Source<streamConnection.ConnectionRequest>
    >
  }
}

export function initServerContext(ctx: time.Context): ServerContext {
  const connectionRequestSources = new collection.Memo<
    string,
    streamSource.Source<streamConnection.ConnectionRequest>
  >(() => new streamSource.Source())

  self.addEventListener('message', (event) => {
    console.log('message', event)
    const connectionRequest =
      event.data as streamConnection.ConnectionRequest
    void connectionRequestSources
      .get(connectionRequest.service)
      .send(ctx, connectionRequest)
  })

  return {
    streamServer: {
      connectionRequestSources: connectionRequestSources,
    },
  }
}

export function serve(
  ctx: ServerContext,
  service: string,
  onConnect: (serverSource: streamSource.Source<unknown>) => Promise<{
    onData: (data: unknown) => Promise<void>
  }>
): void {
  const connectionRequestStream =
    ctx.streamServer.connectionRequestSources.get(service).readable

  const connectionRequestSink =
    new streamSink.Sink<streamConnection.ConnectionRequest>(
      async (connectionRequest) => {
        const serverSource = new streamSource.Source<unknown>()
        const { onData } = await onConnect(serverSource)
        const connectionResponse: streamConnection.ConnectionResponse = {
          type: 'ConnectionResponse',
          connectionId: connectionRequest.connectionId,
          serverStream: serverSource.readable,
        }
        self.postMessage(connectionResponse, [
          connectionResponse.serverStream,
        ])
        const serverSink = new streamSink.Sink(onData)
        void connectionRequest.clientStream.pipeTo(serverSink.writable)
        return Promise.resolve()
      }
    )
  void connectionRequestStream.pipeTo(connectionRequestSink.writable)
}