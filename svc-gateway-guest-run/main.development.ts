import * as test from '@intertwine/lib-test'
import type * as time from '@intertwine/lib-time'
import type * as widget from '@intertwine/lib-widget'

export async function main(
  ctx: widget.Context & time.Context
): Promise<void> {
  await test.runAll(ctx, [import('@intertwine/dev-pnpm-test')])
}
