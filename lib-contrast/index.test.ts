import type * as contrast from '@/index.ts'
import type * as compute from '@intertwine/lib-compute'
import type * as test from '@intertwine/lib-test'

export const all: test.TestCollection<
  compute.Context & contrast.Context
> = () => [
  import('@/expressionIntern.test.ts'),
  import('@/function.test.ts'),
  import('@/props.test.ts'),
  import('@/pseudo.test.ts'),
  import('@/style.test.ts'),
  import('@/var_.test.ts'),
]
