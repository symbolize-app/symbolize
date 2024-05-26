import { Memo } from '@/memo.ts'

export function groupBy<T, K>(
  items: readonly T[],
  selector: (item: T) => K,
): readonly [key: K, groupItems: T[]][] {
  const mutableGroups = new Memo<K, T[]>(() => [])
  for (const item of items) {
    const key = selector(item)
    const mutableGroup = mutableGroups.get(key)
    mutableGroup.push(item)
  }
  return [...mutableGroups.entries()]
}
