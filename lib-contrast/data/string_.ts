import * as collection from '@intertwine/lib-collection'

const stringMarker = Symbol('stringMarker')

/**
 * @see https://developer.mozilla.org/en-US/docs/Web/CSS/string
 */
export interface String_ {
  [stringMarker](): unknown
}

const stringLiteralIntern = new collection.Memo<string, StringLiteral>(
  (text) => new StringLiteral(text),
)

class StringLiteral implements String_ {
  constructor(readonly text: string) {}

  [stringMarker](): unknown {
    return null
  }

  toString(): string {
    return JSON.stringify(this.text)
  }
}

/**
 * @see https://developer.mozilla.org/en-US/docs/Web/CSS/string
 */
export function stringLiteral(text: string): String_ {
  return stringLiteralIntern.get(text)
}

const attrIntern = new collection.Memo<string, Attr>(
  (name) => new Attr(name),
)

class Attr implements String_ {
  constructor(readonly name: string) {}

  [stringMarker](): unknown {
    return null
  }

  toString(): string {
    return `attr(${this.name})`
  }
}

/**
 * @see https://developer.mozilla.org/en-US/docs/Web/CSS/attr
 */
export function attr(name: string): String_ {
  return attrIntern.get(name)
}
