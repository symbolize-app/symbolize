import * as appEndpointMember from '@fe/core/endpoint/member.ts'
import * as random from '@tiny/core/random.ts'
import * as test from '@tiny/test/index.ts'

export const url = import.meta.url

export const tests = {
  ['member create request, ok']: (
    ctx: test.Context
  ): void => {
    const validator = appEndpointMember.create.requestJson
    const input = {
      requestId: random.requestIdHex(ctx),
      email: 'test@example.org',
      handle: 'test',
    }
    test.assertDeepEquals(validator.check(input), input)
  },
  ['member create ok response, ok']: (): void => {
    const validator = appEndpointMember.create.okResponseJson
    const input = {
      id: 'd2f17ea3a0e36a7c79442855ca7d0a71a4eb616e10704121b4d169b6486f3bdc',
    }
    test.assertDeepEquals(validator.check(input), input)
  },
  ['member create conflict response, ok']: (): void => {
    const validator =
      appEndpointMember.create.conflictResponseJson
    const input = { conflict: 'email' }
    test.assertDeepEquals(validator.check(input), input)
  },
}
