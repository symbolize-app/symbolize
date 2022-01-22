import type * as tinyTest from '@tiny/test/index.ts'

export const all: tinyTest.TestCollection = () => [
  import('@fe/core/endpoint/member.test.ts'),
  import('@fe/core/message.test.ts'),
  import('@fe/core/payload.test.ts'),
]
