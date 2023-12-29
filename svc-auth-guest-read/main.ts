import type * as error from '@intertwine/lib-error'
import * as time from '@intertwine/lib-time'

export async function main(ctx: error.Context): Promise<void> {
  await time.delay(ctx, 1)
  console.log('svc-auth-guest-read')
}
