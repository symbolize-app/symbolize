import type * as compute from '@intertwine/lib-compute'
import type * as test from '@intertwine/lib-test'

export const all: test.TestCollection<compute.Context> = () => [
  import('@/atom.test.ts'),
]
