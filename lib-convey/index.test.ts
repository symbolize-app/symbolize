import type * as convey from '@/index.ts'
import type * as compute from '@symbolize/lib-compute'
import type * as contrast from '@symbolize/lib-contrast'
import type * as test from '@symbolize/lib-test'

export const all: test.TestCollection<
  compute.Context & contrast.Context & convey.Context
> = () => [
  import('@/custom.test.ts'),
  import('@/each.test.ts'),
  import('@/empty.test.ts'),
  import('@/html.test.ts'),
  import('@/if_.test.ts'),
  import('@/math.test.ts'),
  import('@/portal.test.ts'),
  import('@/range.test.ts'),
  import('@/select.test.ts'),
  import('@/svg.test.ts'),
  import('@/text.test.ts'),
]
