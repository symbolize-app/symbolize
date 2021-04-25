import * as payload from '@tiny/core/payload.ts'
import * as test from '@tiny/test/index.ts'

export const url = import.meta.url

export const tests = {
  ['checkObject, ok']: (): void => {
    const check = payload.checkObject({
      x: payload.checkObject({}),
    })
    const input = { x: {} }
    test.assertDeepEquals(check(input), input)
  },
  ['checkObject, wrong type']: (): void => {
    const check = payload.checkObject({})
    const error = test.assertThrows(() => check(null))
    test.assertInstanceOf(error, payload.PayloadError)
    test.assertEquals(
      error.message,
      'Invalid object (wrong type null) at (root)'
    )
  },
  ['checkObject, extra key']: (): void => {
    const check = payload.checkObject({})
    const error = test.assertThrows(() => check({ x: 1 }))
    test.assertInstanceOf(error, payload.PayloadError)
    test.assertEquals(
      error.message,
      'Invalid object (extra key "x") at (root)'
    )
  },
  ['checkObject, missing key']: (): void => {
    const check = payload.checkObject({
      x: payload.checkObject({}),
    })
    const error = test.assertThrows(() => check({}))
    test.assertInstanceOf(error, payload.PayloadError)
    test.assertEquals(
      error.message,
      'Invalid object (missing key "x") at (root)'
    )
  },
  ['checkObject, nested missing key']: (): void => {
    const check = payload.checkObject({
      x: payload.checkObject({
        ['space y']: payload.checkObject({
          z: payload.checkObject({}),
        }),
      }),
    })
    const error = test.assertThrows(() =>
      check({ x: { ['space y']: {} } })
    )
    test.assertInstanceOf(error, payload.PayloadError)
    test.assertEquals(
      error.message,
      'Invalid object (missing key "z") at (root).x["space y"]'
    )
  },
  ['checkString, ok']: (): void => {
    const check = payload.checkString({
      min: 2,
      max: 5,
    })
    const input = 'abcde'
    test.assertDeepEquals(check(input), input)
  },
  ['checkString, wrong type']: (): void => {
    const check = payload.checkString({
      min: 2,
      max: 5,
    })
    const error = test.assertThrows(() => check([]))
    test.assertInstanceOf(error, payload.PayloadError)
    test.assertEquals(
      error.message,
      'Invalid string (wrong type array) at (root)'
    )
  },
  ['checkString, too short']: (): void => {
    const check = payload.checkString({
      min: 2,
      max: 5,
    })
    const error = test.assertThrows(() => check('a'))
    test.assertInstanceOf(error, payload.PayloadError)
    test.assertEquals(
      error.message,
      'Invalid string (too short, min 2) at (root)'
    )
  },
  ['checkString, too long']: (): void => {
    const check = payload.checkString({
      min: 2,
      max: 5,
    })
    const error = test.assertThrows(() => check('abcdef'))
    test.assertInstanceOf(error, payload.PayloadError)
    test.assertEquals(
      error.message,
      'Invalid string (too long, max 5) at (root)'
    )
  },
  ['checkStringOptions, ok']: (): void => {
    const check = payload.checkStringOption('abc', 'xyz')
    const input = 'abc'
    test.assertDeepEquals(check(input), input)
  },
  ['checkStringOptions, wrong type']: (): void => {
    const check = payload.checkStringOption('abc', 'xyz')
    const error = test.assertThrows(() => check(2))
    test.assertInstanceOf(error, payload.PayloadError)
    test.assertEquals(
      error.message,
      'Invalid string option (wrong type number) at (root)'
    )
  },
  ['checkStringOptions, wrong option']: (): void => {
    const check = payload.checkStringOption('abc', 'xyz')
    const error = test.assertThrows(() => check('a'))
    test.assertInstanceOf(error, payload.PayloadError)
    test.assertEquals(
      error.message,
      'Invalid string option (not "abc" | "xyz") at (root)'
    )
  },
  ['checkConflictResponse, ok']: (): void => {
    const check = payload.checkConflictResponse(
      'abc',
      'xyz'
    )
    const input = { conflict: 'abc' }
    test.assertDeepEquals(check(input), input)
  },
  ['getTypeName, wrong option']: (): void => {
    test.assertEquals(payload.getTypeName(null), 'null')
    test.assertEquals(payload.getTypeName('a'), 'string')
    test.assertEquals(payload.getTypeName(1), 'number')
    test.assertEquals(payload.getTypeName([]), 'array')
    test.assertEquals(payload.getTypeName({}), 'object')
    test.assertEquals(payload.getTypeName(true), 'boolean')
  },
}
