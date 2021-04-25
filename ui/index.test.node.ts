import type * as test from '@tiny/test/index.ts'
import type * as widget from '@tiny/ui/widget.ts'

export const all: test.TestCollection<widget.Context> = () => [
  import('@fe/ui/widget/button.test.ts'),
  import('@fe/ui/widget/member.test.ts'),
]
