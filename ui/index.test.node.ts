import type * as test from '@tiny/test/index.ts'
import type * as widget from '@tiny/ui/widget.ts'

export const all: test.TestCollection<widget.Context> = () => [
  import('@fe/ui/button.test.ts'),
  import('@fe/ui/member.test.ts'),
]
