import * as test from '@intertwine/lib-test'
import type * as time from '@intertwine/lib-time'
import type * as widget from '@intertwine/lib-widget'

export function main(ctx: widget.Context & time.Context): void {
  test
    .runAll(ctx, [import('@intertwine/dev-pnpm-test')])
    .catch(console.error)
}
