import * as appEndpointMember from '@app/core/endpoint/member.ts'
import * as tinyRandom from '@tiny/core/random.ts'
import * as tinyTest from '@tiny/test/index.ts'

export const url = import.meta.url

export const tests = {
  ['member create request, ok']: (
    ctx: tinyTest.Context
  ): void => {
    const validator = appEndpointMember.create.requestJson
    const input = {
      requestId: tinyRandom.requestIdHex(ctx),
      email: 'test@example.org',
      handle: 'test',
    }
    tinyTest.assertDeepEquals(validator.check(input), input)
  },
  ['member create ok response, ok']: (): void => {
    const validator =
      appEndpointMember.create.okResponseJson
    const input = {
      id: 'd2f17ea3a0e36a7c79442855ca7d0a71a4eb616e10704121b4d169b6486f3bdc',
    }
    tinyTest.assertDeepEquals(validator.check(input), input)
  },
  ['member create conflict response, ok']: (): void => {
    const validator =
      appEndpointMember.create.conflictResponseJson
    const input = { conflict: 'email' }
    tinyTest.assertDeepEquals(validator.check(input), input)
  },
}
