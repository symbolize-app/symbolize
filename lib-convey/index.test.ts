import type * as convey from '@/index.ts'
import type * as compute from '@intertwine/lib-compute'
import type * as test from '@intertwine/lib-test'

export const all: test.TestCollection<
  compute.Context & convey.Context
> = () => [
  import('@/custom.test.ts'),
  import('@/empty.test.ts'),
  import('@/html.test.ts'),
  import('@/items.test.ts'),
  import('@/match.test.ts'),
  import('@/math.test.ts'),
  import('@/range.test.ts'),
  import('@/svg.test.ts'),
  import('@/text.test.ts'),
]
