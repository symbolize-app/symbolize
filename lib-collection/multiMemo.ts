const endMarker = Symbol('endMarker')

type MutableRecursiveMap<K, V> = Map<K, MutableRecursiveMap<K, V> | V>

export class MultiMemo<K extends readonly unknown[], V> {
  private readonly mutableResults: MutableRecursiveMap<K[number], V> =
    new Map()

  constructor(private readonly builder: (...key: K) => V) {}

  get(...key: K): V {
    let mutableResults = this.mutableResults
    for (const subkey of key) {
      if (mutableResults.has(subkey)) {
        // eslint-disable-next-line @typescript-eslint/no-non-null-assertion -- already checked
        mutableResults = mutableResults.get(
          subkey,
        )! as typeof mutableResults
      } else {
        const newResults: typeof mutableResults = new Map()
        mutableResults.set(subkey, newResults)
        mutableResults = newResults
      }
    }
    if (mutableResults.has(endMarker)) {
      // eslint-disable-next-line @typescript-eslint/no-non-null-assertion -- already checked
      return mutableResults.get(endMarker)! as V
    } else {
      const value = this.builder(...key)
      mutableResults.set(endMarker, value)
      return value
    }
  }
}
