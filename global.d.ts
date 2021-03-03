interface ParentNode {
  replaceChildren(
    ...nodesOrDOMStrings: (Node | string)[]
  ): void
}

interface ImportMeta {
  readonly env: {
    readonly [key: string]: string
  }
}
