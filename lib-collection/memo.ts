export class Memo<K, V> {
  private readonly results = new Map<K, V>()

  constructor(private readonly builder: (key: K) => V) {}

  get(key: K): V {
    if (this.results.has(key)) {
      return this.results.get(key)!
    } else {
      const value = this.builder(key)
      this.results.set(key, value)
      return value
    }
  }

  delete(key: K): void {
    this.results.delete(key)
  }
}
