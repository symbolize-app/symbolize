import * as collection from '@intertwine/lib-collection'

export const atomNamespace = 'a'
export const containerNamespace = 'r'
export const symbolNamespace = 's'
export const expressionNamespace = 'e'

export function buildIdentifier(namespace: string, index: number): string {
  return `${namespace}${index.toString(16)}`
}

class IdentifierGenerator {
  private readonly mutableIndex: { value: number } = { value: 0 }

  constructor(private readonly namespace: string) {}

  build(): string {
    return buildIdentifier(this.namespace, this.mutableIndex.value++)
  }
}

export type { IdentifierGenerator }

export function identifierGenerator(
  namespace: string,
): IdentifierGenerator {
  return new IdentifierGenerator(namespace)
}

export function customPropertyNameGenerator(
  namespace: string,
): IdentifierGenerator {
  return identifierGenerator(`--${namespace}`)
}

class IdentifierMemo<Key> {
  private readonly generator: IdentifierGenerator
  private readonly memo: collection.Memo<Key, string>

  constructor(namespace: string) {
    this.generator = new IdentifierGenerator(namespace)
    this.memo = collection.memo(() => this.generator.build())
  }

  entries(): IterableIterator<[Key, string]> {
    return this.memo.entries()
  }

  get(key: Key): string {
    return this.memo.get(key)
  }
}

export type { IdentifierMemo }

export function identifierMemo<Key>(
  namespace: string,
): IdentifierMemo<Key> {
  return new IdentifierMemo(namespace)
}

export function customPropertyNameMemo<Key>(
  namespace: string,
): IdentifierMemo<Key> {
  return identifierMemo(`--${namespace}`)
}
