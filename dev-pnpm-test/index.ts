import type * as test from '@intertwine/lib-test'

export const all: test.TestCollection = () => [
  import('@intertwine/lib-error/index.test.ts'),
  import('@intertwine/lib-payload/index.test.ts'),
  import('@intertwine/lib-test/hex.test.ts'),
  import('@intertwine/lib-test/random.test.ts'),
  import('@intertwine/lib-test/time.test.ts'),
]
