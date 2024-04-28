export const atomNamespace = 'a'
export const expressionNamespace = 'e'

export function buildClassName(namespace: string, index: number): string {
  return `${namespace}${index.toString(16)}`
}

export class CustomPropertyNameGenerator {
  private readonly mutableIndex: { value: number } = { value: 0 }

  constructor(private readonly namespace: string) {}

  build(): string {
    return buildClassName(this.namespace, this.mutableIndex.value++)
  }
}
