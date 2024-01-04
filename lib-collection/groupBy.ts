export function groupBy<T, K>(
  items: T[],
  selector: (item: T) => K
): [key: K, groupItems: T[]][] {
  const groups = new Map<K, T[]>()
  for (const item of items) {
    const key = selector(item)
    if (groups.has(key)) {
      groups.get(key)!.push(item)
    } else {
      groups.set(key, [item])
    }
  }
  return [...groups.entries()]
}