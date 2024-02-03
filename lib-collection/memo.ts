export class Memo<K, V> {
  private readonly results = new Map<K, V>()

  constructor(private readonly builder: (key: K) => V) {}

  delete(key: K): void {
    this.results.delete(key)
  }

  get(key: K): V {
    if (this.results.has(key)) {
      // eslint-disable-next-line @typescript-eslint/no-non-null-assertion
      return this.results.get(key)!
    } else {
      const value = this.builder(key)
      this.results.set(key, value)
      return value
    }
  }
}
