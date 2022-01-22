import type * as tinyRoute from '@tiny/api/route.ts'
import * as tinyTest from '@tiny/test/index.ts'

export function mockReqeuest(
  response: Partial<tinyRoute.Request>
): tinyRoute.Request {
  return {
    origin: 'http://test',
    path: '/',
    match: {},
    params: {},
    method: 'GET',
    headers: {
      'content-type': 'application/json',
    },
    stream: tinyTest.mock([]),
    blob: tinyTest.mock([]),
    buffer: tinyTest.mock([]),
    text: tinyTest.mock([]),
    form: tinyTest.mock([]),
    json: tinyTest.mock([]),
    ...response,
  }
}
