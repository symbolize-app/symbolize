import * as collection from '@intertwine/lib-collection'

export const symbolNamespace = 's'
export const expressionNamespace = 'e'

export function buildCustomPropertyName(
  namespace: string,
  index: number,
): string {
  return `--${namespace}${index.toString(16)}`
}

export class CustomPropertyNameGenerator {
  private readonly mutableIndex: { value: number } = { value: 0 }

  constructor(private readonly namespace: string) {}

  build(): string {
    return buildCustomPropertyName(
      this.namespace,
      this.mutableIndex.value++,
    )
  }
}

export class CustomPropertyNameMemo<Key> {
  private readonly generator: CustomPropertyNameGenerator
  private readonly memo: collection.Memo<Key, string>

  constructor(namespace: string) {
    this.generator = new CustomPropertyNameGenerator(namespace)
    this.memo = new collection.Memo(() => this.generator.build())
  }

  entries(): IterableIterator<[Key, string]> {
    return this.memo.entries()
  }

  get(key: Key): string {
    return this.memo.get(key)
  }
}
