import type * as contrast from '@/index.ts'
import type * as compute from '@intertwine/lib-compute'
import type * as test from '@intertwine/lib-test'

export const all: test.TestCollection<
  compute.Context & contrast.Context
> = () => [
  import('@/atom.test.ts'),
  import('@/container.test.ts'),
  import('@/expressionIntern.test.ts'),
  import('@/function.test.ts'),
  import('@/media.test.ts'),
  import('@/prop/index.test.ts'),
  import('@/pseudoElement.test.ts'),
  import('@/select.test.ts'),
  import('@/support.test.ts'),
  import('@/var_.test.ts'),
]
