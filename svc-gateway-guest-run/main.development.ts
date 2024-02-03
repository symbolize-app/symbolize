import * as test from '@intertwine/lib-test'
import type * as time from '@intertwine/lib-time'

export async function main(ctx: time.Context): Promise<void> {
  await test.runAll(ctx, [import('@intertwine/dev-pnpm-test')])
}
