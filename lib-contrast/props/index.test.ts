import type * as contrast from '@/index.ts'
import type * as compute from '@intertwine/lib-compute'
import type * as test from '@intertwine/lib-test'

export const all: test.TestCollection<
  compute.Context & contrast.Context
> = () => [
  import('@/props/background.test.ts'),
  import('@/props/boxSizing.test.ts'),
  import('@/props/fill.test.ts'),
  import('@/props/font.test.ts'),
]
