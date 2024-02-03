import type * as error from '@intertwine/lib-error'
import * as stream from '@intertwine/lib-stream'

export function main(ctx: error.Context & stream.ClientContext): void {
  const clientSource = stream.connect(
    ctx,
    'svc-auth-guest-read',
    async (data) => {
      // eslint-disable-next-line no-console
      console.log('client data', data)
      return Promise.resolve()
    }
  )
  void clientSource.send(ctx, 'ping')
}
