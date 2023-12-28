export type Memo<K, V> = {
  get(key: K): V
  delete(key: K): void
}

export function memo<K, V>(builder: (key: K) => V): Memo<K, V> {
  const results = new Map<K, V>()
  return {
    get(key) {
      if (results.has(key)) {
        return results.get(key)!
      } else {
        const value = builder(key)
        results.set(key, value)
        return value
      }
    },
    delete(key) {
      results.delete(key)
    },
  }
}
