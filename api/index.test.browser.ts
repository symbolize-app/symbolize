import type * as test from '@tiny/test/index.ts'

export const all: test.TestCollection = () => [
  import('@fe/api/payload.test.ts'),
]