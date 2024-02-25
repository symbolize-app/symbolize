import type * as random from '@intertwine/lib-random'
import type * as stream from '@intertwine/lib-stream'
import type * as time from '@intertwine/lib-time'

export function main(
  ctx: random.Context & stream.ServerContext & time.Context,
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
}
