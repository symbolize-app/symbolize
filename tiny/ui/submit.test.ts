import * as test from '@tiny/test/index.ts'
import type * as submit from '@tiny/ui/submit.ts'

export function mockResponse(
  response: Partial<submit.Response>
): submit.Response {
  return {
    status: 200,
    headers: {},
    stream: test.mock([]),
    buffer: test.mock([]),
    text: test.mock([]),
    json: test.mock([]),
    ...response,
  }
}
