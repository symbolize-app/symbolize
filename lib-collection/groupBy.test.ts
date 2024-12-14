import { groupBy } from '@/groupBy.ts'
import * as test from '@symbolize/lib-test'

export const url = import.meta.url

export const tests = {
  ['empty'](): void {
    test.assertDeepEquals(
      groupBy([], (n) => n),
      [],
    )
  },

  ['basic'](): void {
    test.assertDeepEquals(
      groupBy([1, 1, 2, 5, 8, 9], (n) => n % 2),
      [
        [1, [1, 1, 5, 9]],
        [0, [2, 8]],
      ],
    )
  },
}
