import type * as random from '@intertwine/lib-random'
import * as stream from '@intertwine/lib-stream'
import * as time from '@intertwine/lib-time'

export function main(
  ctx: random.Context &
    stream.Context &
    stream.WorkerServerContext &
    time.Context,
): void {
  ctx.streamServer.serve('svc-auth-guest-read', async (serverSource) => {
    return Promise.resolve({
      async onData(data) {
        // eslint-disable-next-line no-console
        console.log('server data', data)
        await serverSource.send(ctx, 'pong')
      },
    })
  })

  void testStream(ctx)
}

async function testStream(
  ctx: random.Context & stream.Context & time.Context,
): Promise<void> {
  const client = stream.httpClient(
    ctx,
    new URL('/.stream', self.location.origin),
    (data) => {
      // eslint-disable-next-line no-console
      console.log('stream response', data)
    },
  )
  await client.source.send(ctx, 'hello')
  while (true) {
    await time.delay(ctx, 10000)
    await client.source.send(ctx, 'world')
  }
}
