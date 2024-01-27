import type * as test from '@intertwine/lib-test'
import type * as widget from '@intertwine/lib-widget'

export const all: test.TestCollection<widget.Context> = () => [
  import('@intertwine/lib-error/index.test.ts'),
  import('@intertwine/lib-payload/index.test.ts'),
  import('@intertwine/lib-test/hex.test.ts'),
  import('@intertwine/lib-test/random.test.ts'),
  import('@intertwine/lib-test/time.test.ts'),
]
