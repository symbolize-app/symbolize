import type * as compute from '@symbolize/lib-compute'
import type * as contrast from '@symbolize/lib-contrast'
import type * as markup from '@symbolize/lib-markup'
import type * as random from '@symbolize/lib-random'
import type * as stream from '@symbolize/lib-stream'
import type * as test from '@symbolize/lib-test'
import type * as timeTest from '@symbolize/lib-time/test.ts'

export const all: test.TestCollection<
  compute.Context &
    contrast.Context &
    markup.Context &
    random.Context &
    stream.Context &
    timeTest.Context
> = () => [
  import('@symbolize/lib-collection/index.test.ts'),
  import('@symbolize/lib-compute/index.test.ts'),
  import('@symbolize/lib-contrast/index.test.ts'),
  import('@symbolize/lib-error/index.test.ts'),
  import('@symbolize/lib-hex/index.test.ts'),
  import('@symbolize/lib-markup/index.test.ts'),
  import('@symbolize/lib-payload/index.test.ts'),
  import('@symbolize/lib-random/index.test.ts'),
  import('@symbolize/lib-stream/index.test.ts'),
  import('@symbolize/lib-time/index.test.ts'),
]
