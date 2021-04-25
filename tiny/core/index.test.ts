import type * as test from '@tiny/test/index.ts'

export const all: test.TestCollection = () => [
  import('@tiny/core/error.test.ts'),
  import('@tiny/core/hex.test.ts'),
  import('@tiny/core/payload.test.ts'),
  import('@tiny/core/random.test.ts'),
  import('@tiny/core/time.test.ts'),
]
