import type * as time from '@tiny/core/time.ts'
import * as test from '@tiny/test/index.ts'
import type * as widget from '@tiny/ui/widget.ts'

export const all: test.TestCollection<widget.Context> = () => [
  import('@fe/ui/button.test.ts'),
  import('@fe/ui/member.test.ts'),
]

export async function run(
  ctx: widget.Context & time.Context
): Promise<boolean> {
  return await test.runAll(ctx, [
    import('@fe/core/index.test.ts'),
    import('@fe/api/index.test.browser.ts'),
    import('@fe/ui/index.test.browser.ts'),
    import('@tiny/api/index.test.ts'),
    import('@tiny/core/index.test.ts'),
  ])
}
