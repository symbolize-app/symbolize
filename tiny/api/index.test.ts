import type * as test from '@tiny/test/index.ts'

export const all: test.TestCollection = () => [
  import('@tiny/api/payload.test.ts'),
]
