import * as collection from '@symbolize/lib-collection'

const angleMarker = Symbol('angleMarker')

const angleIntern = collection.multiMemo<
  [value: number, unit: AngleUnit],
  Angle
>((value, unit) => new Angle(value, unit))

/**
 * @see https://developer.mozilla.org/en-US/docs/Web/CSS/angle
 */
class Angle<Unit extends AngleUnit = AngleUnit> {
  constructor(
    readonly value: number,
    readonly unit: Unit,
  ) {}

  [angleMarker](): unknown {
    return null
  }

  toString(): string {
    return `${this.value}${this.unit}`
  }
}

export type { Angle }

export function angle<Unit extends AngleUnit>(
  value: number,
  unit: Unit,
): Angle<Unit> {
  return angleIntern.get(value, unit) as Angle<Unit>
}

export const deg = angleHelper('deg')

function angleHelper<Unit extends AngleUnit>(
  unit: Unit,
): (value: number) => Angle<Unit> {
  return (value) => angle(value, unit)
}

export type AngleUnit = 'deg' | 'grad' | 'rad' | 'turn'
