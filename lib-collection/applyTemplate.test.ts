import { applyTemplate } from '@/applyTemplate.ts'
import * as test from '@intertwine/lib-test'

export const url = import.meta.url

const parameterPattern = /\$\{(?<parameterName>[^}]+)\}/g

export const tests = {
  ['empty'](): void {
    test.assertEquals(applyTemplate(parameterPattern, '', {}), '')
    test.assertEquals(applyTemplate(parameterPattern, 'abc', {}), 'abc')
  },

  ['basic'](): void {
    test.assertEquals(
      applyTemplate(parameterPattern, 'aaa${b}ccc', { b: 'q' }),
      'aaaqccc',
    )
  },

  ['multiple'](): void {
    test.assertEquals(
      applyTemplate(parameterPattern, '0${a} 1${b} 2${a}', {
        a: 'A',
        b: 'B',
      }),
      '0A 1B 2A',
    )
  },

  ['error'](): void {
    const error = test.assertThrows(() =>
      applyTemplate(parameterPattern, '0${a} 1${b} 2${c}', {
        a: 'A',
        b: 'B',
      }),
    )
    test.assertInstanceOf(error, Error)
    test.assertEquals(error.message, 'Parameter c not defined')
  },
}
