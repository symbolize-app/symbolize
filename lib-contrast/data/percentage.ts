import * as collection from '@symbolize/lib-collection'

const percentageMarker = Symbol('percentageMarker')

const percentageIntern = collection.memo<number, Pct>(
  (value) => new Pct(value),
)

/**
 * @see https://developer.mozilla.org/en-US/docs/Web/CSS/percentage
 */
class Pct {
  constructor(readonly value: number) {}

  [percentageMarker](): unknown {
    return null
  }

  toString(): string {
    return `${this.value}%`
  }
}

export type { Pct }

export function pct(value: number): Pct {
  return percentageIntern.get(value)
}
