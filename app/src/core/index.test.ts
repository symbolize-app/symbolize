import type * as tinyTest from '@tiny/test/index.ts'

export const all: tinyTest.TestCollection = () => [
  import('@app/core/endpoint/member.test.ts'),
  import('@app/core/message.test.ts'),
  import('@app/core/payload.test.ts'),
]
