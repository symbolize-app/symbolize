export function groupBy<T, K>(
  items: readonly T[],
  selector: (item: T) => K
): readonly [key: K, groupItems: T[]][] {
  const mutableGroups = new Map<K, T[]>()
  for (const item of items) {
    const key = selector(item)
    if (mutableGroups.has(key)) {
      // eslint-disable-next-line @typescript-eslint/no-non-null-assertion
      const mutableGroup = mutableGroups.get(key)!
      mutableGroup.push(item)
    } else {
      mutableGroups.set(key, [item])
    }
  }
  return [...mutableGroups.entries()]
}
