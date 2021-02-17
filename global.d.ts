interface ParentNode {
  replaceChildren(
    ...nodesOrDOMStrings: (Node | string)[]
  ): void
}
