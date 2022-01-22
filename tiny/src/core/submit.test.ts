import type * as tinySubmit from '@tiny/core/submit.ts'
import * as tinyTest from '@tiny/test/index.ts'

export function mockResponse(
  response: Partial<tinySubmit.Response>
): tinySubmit.Response {
  return {
    status: 200,
    headers: {},
    stream: tinyTest.mock([]),
    blob: tinyTest.mock([]),
    buffer: tinyTest.mock([]),
    text: tinyTest.mock([]),
    form: tinyTest.mock([]),
    json: tinyTest.mock([]),
    ...response,
  }
}
