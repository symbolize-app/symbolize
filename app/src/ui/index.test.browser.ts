import type * as tinyTime from '@tiny/core/time.ts'
import * as tinyTest from '@tiny/test/index.ts'
import type * as tinyWidget from '@tiny/ui/widget.ts'

export const all: tinyTest.TestCollection<
  tinyWidget.Context
> = () => [
  import('@app/ui/widget/button.test.ts'),
  import('@app/ui/widget/member.test.ts'),
]

export async function run(
  ctx: tinyWidget.Context & tinyTime.Context
): Promise<boolean> {
  return await tinyTest.runAll(ctx, [
    import('@app/core/index.test.ts'),
    import('@app/ui/index.test.browser.ts'),
    import('@tiny/core/index.test.ts'),
  ])
}
