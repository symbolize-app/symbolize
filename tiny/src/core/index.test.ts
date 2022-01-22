import type * as tinyTest from '@tiny/test/index.ts'

export const all: tinyTest.TestCollection = () => [
  import('@tiny/core/error.test.ts'),
  import('@tiny/core/hex.test.ts'),
  import('@tiny/core/payload.test.ts'),
  import('@tiny/core/random.test.ts'),
  import('@tiny/core/time.test.ts'),
]
