import type * as submit from '@tiny/core/submit.ts'
import * as test from '@tiny/test/index.ts'

export function mockResponse(
  response: Partial<submit.Response>
): submit.Response {
  return {
    status: 200,
    headers: {},
    stream: test.mock([]),
    buffer: test.mock([]),
    text: test.mock([]),
    form: test.mock([]),
    json: test.mock([]),
    ...response,
  }
}
