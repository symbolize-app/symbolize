import * as tinyTest from '@intertwine/test/index.ts'
import type * as tinyTime from '@intertwine/time/time.ts'
import type * as tinyWidget from '@intertwine/widget/widget.ts'

export const all: tinyTest.TestCollection<
  tinyWidget.Context
> = () => [
  import('@/widget/button.test.ts'),
  import('@/widget/member.test.ts'),
]

export async function run(
  ctx: tinyWidget.Context & tinyTime.Context
): Promise<boolean> {
  return await tinyTest.runAll(ctx, [
    import('@/index.test.browser.ts'),
  ])
}
