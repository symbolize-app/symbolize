import type * as streamContext from '@/context.ts'
import type * as random from '@intertwine/lib-random'
import type * as test from '@intertwine/lib-test'
import type * as timeTest from '@intertwine/lib-time/test.ts'

export const all: test.TestCollection<
  random.Context & streamContext.Context & timeTest.Context
> = () => [import('@/http/client.test.ts')]
