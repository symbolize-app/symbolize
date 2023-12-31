import type * as error from '@intertwine/lib-error'
import * as stream from '@intertwine/lib-stream'

export function main(ctx: error.Context & stream.ServerContext): void {
  stream.serve(ctx, 'svc-auth-guest-read', (serverSource) => {
    return Promise.resolve({
      async onData(data) {
        console.log('server data', data)
        await serverSource.send(ctx, 'pong')
      },
    })
  })
}
