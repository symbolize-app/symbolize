export class Length<Unit extends LengthUnit = LengthUnit> {
  constructor(
    readonly value: number,
    readonly unit: Unit,
  ) {}

  toString(): string {
    return `${this.value}${this.unit}`
  }
}

export function length<Unit extends LengthUnit>(
  value: number,
  unit: Unit,
): Length<Unit> {
  return new Length(value, unit)
}

export const percent = lengthHelper('%')

export const em = lengthHelper('em')

export const pt = lengthHelper('pt')

export const px = lengthHelper('px')

export const rem = lengthHelper('rem')

export const vh = lengthHelper('vh')

export const vw = lengthHelper('vw')

function lengthHelper<Unit extends LengthUnit>(
  unit: Unit,
): (value: number) => Length<Unit> {
  return (value) => new Length(value, unit)
}

export type LengthUnit =
  | '%'
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
