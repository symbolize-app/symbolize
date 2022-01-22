import * as tinyPayload from '@tiny/core/payload.ts'
import * as tinyTest from '@tiny/test/index.ts'

export const url = import.meta.url

export const tests = {
  ['null, ok']: (): void => {
    const validator = tinyPayload.object({
      x: tinyPayload.nullOr(tinyPayload.object({})),
      y: tinyPayload.nullOr(tinyPayload.object({})),
    })
    const input = { x: null, y: {} }
    tinyTest.assertDeepEquals(validator.check(input), input)
  },
  ['object, ok']: (): void => {
    const validator = tinyPayload.object({
      x: tinyPayload.object({}),
    })
    const input = { x: {} }
    tinyTest.assertDeepEquals(validator.check(input), input)
  },
  ['object, wrong type']: (): void => {
    const validator = tinyPayload.object({})
    const error = tinyTest.assertThrows(() =>
      validator.check(null)
    )
    tinyTest.assertInstanceOf(
      error,
      tinyPayload.PayloadError
    )
    tinyTest.assertEquals(
      error.message,
      'Invalid object (wrong type null) at (root)'
    )
  },
  ['object, extra key ok']: (): void => {
    const validator = tinyPayload.object({})
    const input = { x: {} }
    tinyTest.assertDeepEquals(validator.check(input), {})
  },
  ['object, missing key']: (): void => {
    const validator = tinyPayload.object({
      x: tinyPayload.object({}),
    })
    const error = tinyTest.assertThrows(() =>
      validator.check({})
    )
    tinyTest.assertInstanceOf(
      error,
      tinyPayload.PayloadError
    )
    tinyTest.assertEquals(
      error.message,
      'Invalid object (missing key "x") at (root)'
    )
  },
  ['object, nested missing key']: (): void => {
    const validator = tinyPayload.object({
      x: tinyPayload.object({
        ['space y']: tinyPayload.object({
          z: tinyPayload.object({}),
        }),
      }),
    })
    const error = tinyTest.assertThrows(() =>
      validator.check({ x: { ['space y']: {} } })
    )
    tinyTest.assertInstanceOf(
      error,
      tinyPayload.PayloadError
    )
    tinyTest.assertEquals(
      error.message,
      'Invalid object (missing key "z") at (root).x["space y"]'
    )
  },
  ['array, ok']: (): void => {
    const validator = tinyPayload.array(
      tinyPayload.object({})
    )
    const input = [{}]
    tinyTest.assertDeepEquals(validator.check(input), input)
  },
  ['array, wrong type']: (): void => {
    const validator = tinyPayload.array(
      tinyPayload.object({})
    )
    const error = tinyTest.assertThrows(() =>
      validator.check(null)
    )
    tinyTest.assertInstanceOf(
      error,
      tinyPayload.PayloadError
    )
    tinyTest.assertEquals(
      error.message,
      'Invalid array (wrong type null) at (root)'
    )
  },
  ['array, nested missing key']: (): void => {
    const validator = tinyPayload.array(
      tinyPayload.object({
        ['space y']: tinyPayload.object({
          z: tinyPayload.object({}),
        }),
      })
    )
    const error = tinyTest.assertThrows(() =>
      validator.check([{ ['space y']: {} }])
    )
    tinyTest.assertInstanceOf(
      error,
      tinyPayload.PayloadError
    )
    tinyTest.assertEquals(
      error.message,
      'Invalid object (missing key "z") at (root)[0]["space y"]'
    )
  },
  ['string, ok']: (): void => {
    const validator = tinyPayload.string({
      min: 2,
      max: 5,
    })
    const input = 'abcde'
    tinyTest.assertDeepEquals(validator.check(input), input)
  },
  ['string, wrong type']: (): void => {
    const validator = tinyPayload.string({
      min: 2,
      max: 5,
    })
    const error = tinyTest.assertThrows(() =>
      validator.check([])
    )
    tinyTest.assertInstanceOf(
      error,
      tinyPayload.PayloadError
    )
    tinyTest.assertEquals(
      error.message,
      'Invalid string (wrong type array) at (root)'
    )
  },
  ['string, too short']: (): void => {
    const validator = tinyPayload.string({
      min: 2,
      max: 5,
    })
    const error = tinyTest.assertThrows(() =>
      validator.check('a')
    )
    tinyTest.assertInstanceOf(
      error,
      tinyPayload.PayloadError
    )
    tinyTest.assertEquals(
      error.message,
      'Invalid string (too short, min 2) at (root)'
    )
  },
  ['string, too long']: (): void => {
    const validator = tinyPayload.string({
      min: 2,
      max: 5,
    })
    const error = tinyTest.assertThrows(() =>
      validator.check('abcdef')
    )
    tinyTest.assertInstanceOf(
      error,
      tinyPayload.PayloadError
    )
    tinyTest.assertEquals(
      error.message,
      'Invalid string (too long, max 5) at (root)'
    )
  },
  ['stringOptions, ok']: (): void => {
    const validator = tinyPayload.stringOption('abc', 'xyz')
    const input = 'abc'
    tinyTest.assertDeepEquals(validator.check(input), input)
  },
  ['stringOptions, wrong type']: (): void => {
    const validator = tinyPayload.stringOption('abc', 'xyz')
    const error = tinyTest.assertThrows(() =>
      validator.check(2)
    )
    tinyTest.assertInstanceOf(
      error,
      tinyPayload.PayloadError
    )
    tinyTest.assertEquals(
      error.message,
      'Invalid string option (wrong type number) at (root)'
    )
  },
  ['stringOptions, wrong option']: (): void => {
    const validator = tinyPayload.stringOption('abc', 'xyz')
    const error = tinyTest.assertThrows(() =>
      validator.check('a')
    )
    tinyTest.assertInstanceOf(
      error,
      tinyPayload.PayloadError
    )
    tinyTest.assertEquals(
      error.message,
      'Invalid string option (not "abc" | "xyz") at (root)'
    )
  },
  ['stringEnum, ok']: (): void => {
    enum Test {
      x = 'x',
      y = 'y2',
    }
    const validator = tinyPayload.stringEnum(Test)
    const input = 'y2'
    tinyTest.assertDeepEquals(
      validator.check(input),
      Test.y
    )
  },
  ['stringEnum, wrong enum']: (): void => {
    enum Test {
      x = 'x',
      y = 'y2',
    }
    const validator = tinyPayload.stringEnum(Test)
    const error = tinyTest.assertThrows(() =>
      validator.check('y')
    )
    tinyTest.assertInstanceOf(
      error,
      tinyPayload.PayloadError
    )
    tinyTest.assertEquals(
      error.message,
      'Invalid enum (not "x" | "y2") at (root)'
    )
  },
  ['conflictResponse, ok']: (): void => {
    const validator = tinyPayload.conflict('abc', 'xyz')
    const input = { conflict: 'abc' }
    tinyTest.assertDeepEquals(validator.check(input), input)
  },
  ['getTypeName, wrong option']: (): void => {
    tinyTest.assertEquals(
      tinyPayload.getTypeName(null),
      'null'
    )
    tinyTest.assertEquals(
      tinyPayload.getTypeName('a'),
      'string'
    )
    tinyTest.assertEquals(
      tinyPayload.getTypeName(1),
      'number'
    )
    tinyTest.assertEquals(
      tinyPayload.getTypeName([]),
      'array'
    )
    tinyTest.assertEquals(
      tinyPayload.getTypeName({}),
      'object'
    )
    tinyTest.assertEquals(
      tinyPayload.getTypeName(true),
      'boolean'
    )
  },
}
