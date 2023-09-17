import type * as tinyTest from '@intertwine/lib-test/index.ts'
import type * as tinyWidget from '@intertwine/lib-widget/widget.ts'

export const all: tinyTest.TestCollection<
  tinyWidget.Context
> = () => [
  import('@/widget/button.test.ts'),
  import('@/widget/member.test.ts'),
]
