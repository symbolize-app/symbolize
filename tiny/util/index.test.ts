import type * as test from '@tiny/test/index.ts'

export const all: test.TestCollection = () => [
  import('@tiny/util/hex.test.ts'),
  import('@tiny/util/error.test.ts'),
  import('@tiny/util/random.test.ts'),
  import('@tiny/util/time.test.ts'),
]
