import { Memo } from '@/memo.ts'
import * as test from '@intertwine/lib-test'

export const url = import.meta.url

export const tests = {
  ['get'](): void {
    const [generate, generateHistory] = test.repeatMockWithHistory(
      2,
      (n: number) => n * 2,
    )
    const memo = new Memo<number, number>(generate)
    test.assertDeepEquals(generateHistory, [])
    test.assertEquals(memo.get(1), 2)
    test.assertDeepEquals(generateHistory, [[1]])
    test.assertEquals(memo.get(1), 2)
    test.assertDeepEquals(generateHistory, [[1]])
    test.assertEquals(memo.get(2), 4)
    test.assertDeepEquals(generateHistory, [[1], [2]])
  },

  ['delete'](): void {
    const [generate, generateHistory] = test.repeatMockWithHistory(
      2,
      (n: number) => n * 2,
    )
    const memo = new Memo<number, number>(generate)
    test.assertDeepEquals(generateHistory, [])
    memo.get(1)
    test.assertDeepEquals(generateHistory, [[1]])
    memo.delete(1)
    memo.get(1)
    test.assertDeepEquals(generateHistory, [[1], [1]])
  },

  ['entries'](): void {
    const memo = new Memo<number, number>((n) => n * 2)
    test.assertDeepEquals([...memo.entries()], [])
    memo.get(1)
    test.assertDeepEquals([...memo.entries()], [[1, 2]])
    memo.get(4)
    test.assertDeepEquals(
      [...memo.entries()],
      [
        [1, 2],
        [4, 8],
      ],
    )
    memo.get(2)
    test.assertDeepEquals(
      [...memo.entries()],
      [
        [1, 2],
        [4, 8],
        [2, 4],
      ],
    )
  },
}
