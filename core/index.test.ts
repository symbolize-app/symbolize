import type * as test from '@tiny/test/index.ts'

export const all: test.TestCollection = () => [
  import('@fe/core/endpoint.test.ts'),
  import('@fe/core/message.test.ts'),
  import('@fe/core/payload.test.ts'),
]
