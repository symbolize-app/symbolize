import type * as styling from '@/index.ts'
import type * as compute from '@symbolize/lib-compute'
import type * as test from '@symbolize/lib-test'

export const all: test.TestCollection<
  compute.Context & styling.Context
> = () => [
  import('@/atom.test.ts'),
  import('@/container.test.ts'),
  import('@/expressionIntern.test.ts'),
  import('@/math.test.ts'),
  import('@/media.test.ts'),
  import('@/prop/index.test.ts'),
  import('@/pseudoElement.test.ts'),
  import('@/select.test.ts'),
  import('@/support.test.ts'),
  import('@/var_.test.ts'),
]
