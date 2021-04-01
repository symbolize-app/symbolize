import type * as submit from '@tiny/api/submit.ts'
import * as test from '@tiny/test/index.ts'

export function mockResponse(
  response: Partial<submit.Response>
): submit.Response {
  return {
    status: 200,
    headers: {},
    buffer: test.mock([]),
    text: test.mock([]),
    json: test.mock([]),
    ...response,
  }
}
