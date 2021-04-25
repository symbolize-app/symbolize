import type * as test from '@tiny/test/index.ts'

// TODO Move util to core

export const all: test.TestCollection = () => [
  import('@tiny/core/hex.test.ts'),
  import('@tiny/core/error.test.ts'),
  import('@tiny/core/random.test.ts'),
  import('@tiny/core/time.test.ts'),
]
