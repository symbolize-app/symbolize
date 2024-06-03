import type * as test from '@intertwine/lib-test'

export const all: test.TestCollection = () => [
  import('@/applyTemplate.test.ts'),
  import('@/groupBy.test.ts'),
  import('@/memo.test.ts'),
  import('@/multiMemo.test.ts'),
  import('@/stripPrefix.test.ts'),
]
