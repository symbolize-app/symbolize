import { multiMemo } from '@/multiMemo.ts'
import * as test from '@symbolize/lib-test'

export const url = import.meta.url

export const tests = {
  ['get basic'](): void {
    const [generate, generateHistory] = test.repeatMockWithHistory(
      2,
      (a: number, b: number, c: number) => a + b + c,
    )
    const container = multiMemo(generate)
    test.assertDeepEquals(generateHistory, [])
    test.assertEquals(container.get(1, 2, 3), 6)
    test.assertDeepEquals(generateHistory, [[1, 2, 3]])
    test.assertEquals(container.get(1, 2, 3), 6)
    test.assertDeepEquals(generateHistory, [[1, 2, 3]])
    test.assertEquals(container.get(2, 2, 3), 7)
    test.assertDeepEquals(generateHistory, [
      [1, 2, 3],
      [2, 2, 3],
    ])
    test.assertEquals(container.get(2, 2, 3), 7)
    test.assertDeepEquals(generateHistory, [
      [1, 2, 3],
      [2, 2, 3],
    ])
  },

  ['get variable'](): void {
    const [generate, generateHistory] = test.repeatMockWithHistory(
      3,
      (...n: readonly number[]) => n.reduce((a, b) => a + b, 0),
    )
    const memo = multiMemo(generate)
    test.assertDeepEquals(generateHistory, [])
    test.assertEquals(memo.get(), 0)
    test.assertDeepEquals(generateHistory, [[]])
    test.assertEquals(memo.get(), 0)
    test.assertDeepEquals(generateHistory, [[]])
    test.assertEquals(memo.get(1), 1)
    test.assertDeepEquals(generateHistory, [[], [1]])
    test.assertEquals(memo.get(1), 1)
    test.assertDeepEquals(generateHistory, [[], [1]])
    test.assertEquals(memo.get(1, 2, 3), 6)
    test.assertDeepEquals(generateHistory, [[], [1], [1, 2, 3]])
    test.assertEquals(memo.get(1, 2, 3), 6)
    test.assertDeepEquals(generateHistory, [[], [1], [1, 2, 3]])
  },
}
