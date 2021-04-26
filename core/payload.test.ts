import * as appPayload from '@fe/core/payload.ts'
import * as test from '@tiny/test/index.ts'

export const url = import.meta.url

export const tests = {
  ['id, ok']: (): void => {
    const check = appPayload.checkId
    const input =
      'd2f17ea3a0e36a7c79442855ca7d0a71a4eb616e10704121b4d169b6486f3bdc'
    test.assertDeepEquals(check(input), input)
  },
  ['email, ok']: (): void => {
    const check = appPayload.checkEmail
    const input =
      'a@b.c'
    test.assertDeepEquals(check(input), input)
  },
  ['handle, ok']: (): void => {
    const check = appPayload.checkHandle
    const input =
      'xyz'
    test.assertDeepEquals(check(input), input)
  },
  ['title, ok']: (): void => {
    const check = appPayload.checkTitle
    const input =
      'the'
    test.assertDeepEquals(check(input), input)
  },
  ['slug, ok']: (): void => {
    const check = appPayload.checkSlug
    const input =
      'oth'
    test.assertDeepEquals(check(input), input)
  },
  ['content, ok']: (): void => {
    const check = appPayload.checkContent
    const input =
      'yzx'
    test.assertDeepEquals(check(input), input)
  },
}
