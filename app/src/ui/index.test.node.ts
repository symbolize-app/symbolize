import type * as tinyTest from '@tiny/test/index.ts'
import type * as tinyWidget from '@tiny/ui/widget.ts'

export const all: tinyTest.TestCollection<
  tinyWidget.Context
> = () => [
  import('@app/ui/widget/button.test.ts'),
  import('@app/ui/widget/member.test.ts'),
]
