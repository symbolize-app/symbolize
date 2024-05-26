import { stripPrefix } from '@/stripPrefix.ts'
import * as test from '@intertwine/lib-test'

export const url = import.meta.url

export const tests = {
  ['empty'](): void {
    test.assertEquals(stripPrefix('', /a+/), null)
  },

  ['basic'](): void {
    test.assertEquals(stripPrefix('aaabc', /a+/), 'bc')
  },
}
