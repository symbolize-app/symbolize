import * as appPayload from '@fe/core/payload.ts'
import * as test from '@tiny/test/index.ts'

export const url = import.meta.url

export const tests = {
  ['id, ok']: (): void => {
    const validator = appPayload.id
    const input =
      'd2f17ea3a0e36a7c79442855ca7d0a71a4eb616e10704121b4d169b6486f3bdc'
    test.assertDeepEquals(validator.check(input), input)
  },
  ['email, ok']: (): void => {
    const validator = appPayload.email
    const input = 'a@b.c'
    test.assertDeepEquals(validator.check(input), input)
  },
  ['handle, ok']: (): void => {
    const validator = appPayload.handle
    const input = 'xyz'
    test.assertDeepEquals(validator.check(input), input)
  },
  ['title, ok']: (): void => {
    const validator = appPayload.title
    const input = 'the'
    test.assertDeepEquals(validator.check(input), input)
  },
  ['slug, ok']: (): void => {
    const validator = appPayload.slug
    const input = 'oth'
    test.assertDeepEquals(validator.check(input), input)
  },
  ['content, ok']: (): void => {
    const validator = appPayload.content
    const input = 'yzx'
    test.assertDeepEquals(validator.check(input), input)
  },
}
