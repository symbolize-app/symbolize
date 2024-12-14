import type * as streamContext from '@/context.ts'
import type * as random from '@symbolize/lib-random'
import type * as test from '@symbolize/lib-test'
import type * as timeTest from '@symbolize/lib-time/test.ts'

export const all: test.TestCollection<
  random.Context & streamContext.Context & timeTest.Context
> = () => [import('@/http/client.test.ts')]
