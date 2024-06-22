import * as collection from '@intertwine/lib-collection'

const percentageMarker = Symbol('percentageMarker')

const percentageIntern = new collection.Memo<number, Pct>(
  (value) => new Pct(value),
)

/**
 * @see https://developer.mozilla.org/en-US/docs/Web/CSS/percentage
 */
export class Pct {
  constructor(readonly value: number) {}

  [percentageMarker](): unknown {
    return null
  }

  toString(): string {
    return `${this.value}%`
  }
}

export function pct(value: number): Pct {
  return percentageIntern.get(value)
}
