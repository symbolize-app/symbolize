import * as appPayload from '@fe/core/payload.ts'
import * as tinyTest from '@tiny/test/index.ts'

export const url = import.meta.url

export const tests = {
  ['id, ok']: (): void => {
    const validator = appPayload.id
    const input =
      'd2f17ea3a0e36a7c79442855ca7d0a71a4eb616e10704121b4d169b6486f3bdc'
    tinyTest.assertDeepEquals(validator.check(input), input)
  },
  ['email, ok']: (): void => {
    const validator = appPayload.email
    const input = 'a@b.c'
    tinyTest.assertDeepEquals(validator.check(input), input)
  },
  ['handle, ok']: (): void => {
    const validator = appPayload.handle
    const input = 'xyz'
    tinyTest.assertDeepEquals(validator.check(input), input)
  },
  ['title, ok']: (): void => {
    const validator = appPayload.title
    const input = 'the'
    tinyTest.assertDeepEquals(validator.check(input), input)
  },
  ['slug, ok']: (): void => {
    const validator = appPayload.slug
    const input = 'oth'
    tinyTest.assertDeepEquals(validator.check(input), input)
  },
  ['content, ok']: (): void => {
    const validator = appPayload.content
    const input = 'yzx'
    tinyTest.assertDeepEquals(validator.check(input), input)
  },
}
