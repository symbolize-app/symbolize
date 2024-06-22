import * as collection from '@intertwine/lib-collection'

const lengthMarker = Symbol('lengthMarker')

const lengthIntern = new collection.MultiMemo<
  [value: number, unit: LengthUnit],
  Length
>((value, unit) => new Length(value, unit))

/**
 * @see https://developer.mozilla.org/en-US/docs/Web/CSS/length
 */
export class Length<Unit extends LengthUnit = LengthUnit> {
  constructor(
    readonly value: number,
    readonly unit: Unit,
  ) {}

  [lengthMarker](): unknown {
    return null
  }

  toString(): string {
    return `${this.value}${this.unit}`
  }
}

export function length<Unit extends LengthUnit>(
  value: number,
  unit: Unit,
): Length<Unit> {
  return lengthIntern.get(value, unit) as Length<Unit>
}

export type Em = Length<'em'>

export const em = lengthHelper('em')

export type Lh = Length<'lh'>

export const lh = lengthHelper('lh')

export type Pt = Length<'pt'>

export const pt = lengthHelper('pt')

export type Px = Length<'px'>

export const px = lengthHelper('px')

export type Rem = Length<'rem'>

export const rem = lengthHelper('rem')

export type Rlh = Length<'rlh'>

export const rlh = lengthHelper('rlh')

function lengthHelper<Unit extends LengthUnit>(
  unit: Unit,
): (value: number) => Length<Unit> {
  return (value) => length(value, unit)
}

export type LengthUnit =
  | 'cap'
  | 'ch'
  | 'cm'
  | 'dvb'
  | 'dvh'
  | 'dvi'
  | 'dvmax'
  | 'dvmin'
  | 'dvw'
  | 'em'
  | 'ex'
  | 'ic'
  | 'in'
  | 'lh'
  | 'lvb'
  | 'lvh'
  | 'lvi'
  | 'lvmax'
  | 'lvmin'
  | 'lvw'
  | 'mm'
  | 'pc'
  | 'pt'
  | 'px'
  | 'Q'
  | 'rcap'
  | 'rch'
  | 'rem'
  | 'rex'
  | 'ric'
  | 'rlh'
  | 'svb'
  | 'svh'
  | 'svi'
  | 'svmax'
  | 'svmin'
  | 'svw'
  | 'vb'
  | 'vh'
  | 'vi'
  | 'vmax'
  | 'vmin'
  | 'vw'
