import { memo } from '@/memo.ts'
import * as test from '@intertwine/lib-test'

export const url = import.meta.url

export const tests = {
  ['get'](): void {
    const [generate, generateHistory] = test.repeatMockWithHistory(
      2,
      (n: number) => n * 2,
    )
    const container = memo<number, number>(generate)
    test.assertDeepEquals(generateHistory, [])
    test.assertEquals(container.get(1), 2)
    test.assertDeepEquals(generateHistory, [[1]])
    test.assertEquals(container.get(1), 2)
    test.assertDeepEquals(generateHistory, [[1]])
    test.assertEquals(container.get(2), 4)
    test.assertDeepEquals(generateHistory, [[1], [2]])
  },

  ['delete'](): void {
    const [generate, generateHistory] = test.repeatMockWithHistory(
      2,
      (n: number) => n * 2,
    )
    const container = memo<number, number>(generate)
    test.assertDeepEquals(generateHistory, [])
    container.get(1)
    test.assertDeepEquals(generateHistory, [[1]])
    container.delete(1)
    container.get(1)
    test.assertDeepEquals(generateHistory, [[1], [1]])
  },

  ['entries'](): void {
    const container = memo<number, number>((n) => n * 2)
    test.assertDeepEquals([...container.entries()], [])
    test.assertDeepEquals([...container.keys()], [])
    test.assertDeepEquals([...container.values()], [])
    container.get(1)
    test.assertDeepEquals([...container.entries()], [[1, 2]])
    test.assertDeepEquals([...container.keys()], [1])
    test.assertDeepEquals([...container.values()], [2])
    container.get(4)
    test.assertDeepEquals(
      [...container.entries()],
      [
        [1, 2],
        [4, 8],
      ],
    )
    test.assertDeepEquals([...container.keys()], [1, 4])
    test.assertDeepEquals([...container.values()], [2, 8])
    container.get(2)
    test.assertDeepEquals(
      [...container.entries()],
      [
        [1, 2],
        [4, 8],
        [2, 4],
      ],
    )
    test.assertDeepEquals([...container.keys()], [1, 4, 2])
    test.assertDeepEquals([...container.values()], [2, 8, 4])
  },
}
