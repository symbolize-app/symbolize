import * as apiPayload from '@fe/api/payload.ts'
import * as random from '@tiny/core/random.ts'
import * as test from '@tiny/test/index.ts'

export const url = import.meta.url

export const tests = {
  ['checkMemberCreateRequest, ok']: (
    ctx: test.Context
  ): void => {
    const check = apiPayload.checkMemberCreateRequest
    const input = {
      requestId: random.requestIdHex(ctx),
      email: 'test@example.org',
      handle: 'test',
    }
    test.assertDeepEquals(check(input), input)
  },
  ['checkMemberCreateResponse, ok']: (): void => {
    const check = apiPayload.checkMemberCreateOkResponse
    const input = {
      id:
        'd2f17ea3a0e36a7c79442855ca7d0a71a4eb616e10704121b4d169b6486f3bdc',
    }
    test.assertDeepEquals(check(input), input)
  },
  ['checkMemberCreateConflictResponse, ok']: (): void => {
    const check =
      apiPayload.checkMemberCreateConflictResponse
    const input = { conflict: 'email' }
    test.assertDeepEquals(check(input), input)
  },
}
