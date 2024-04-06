export function svgPreserveAspectRatio(data: {
  readonly align: SvgPreserveAspectRatio['align']
  readonly mode?: SvgPreserveAspectRatio['mode']
}): SvgPreserveAspectRatioOpt {
  if (data.mode) {
    return new SvgPreserveAspectRatio(data.align, data.mode)
  } else {
    return data.align
  }
}

export type SvgPreserveAspectRatioOpt =
  | SvgPreserveAspectRatio
  | SvgPreserveAspectRatio['align']

export class SvgPreserveAspectRatio {
  constructor(
    readonly align:
      | 'none'
      | 'xMaxYMax'
      | 'xMaxYMid'
      | 'xMaxYMin'
      | 'xMidYMax'
      | 'xMidYMid'
      | 'xMidYMin'
      | 'xMinYMax'
      | 'xMinYMid'
      | 'xMinYMin',
    readonly mode: 'meet' | 'slice',
  ) {}

  toString(): string {
    return `${this.align} ${this.mode}`
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

export class Length<Unit extends LengthUnit = LengthUnit> {
  constructor(
    readonly value: number,
    readonly unit: Unit,
  ) {}

  toString(): string {
    return `${this.value}${this.unit}`
  }
}

export type SvgLengthOpt = SvgLength | SvgLength['value']

export type SvgLengthUnit =
  | '%'
  | 'cm'
  | 'em'
  | 'ex'
  | 'in'
  | 'mm'
  | 'pc'
  | 'pt'
  | 'px'

export type SvgLength = Length<SvgLengthUnit>

export function rect(data: {
  readonly height: number
  readonly left: number
  readonly top: number
  readonly width: number
}): Rect {
  return [data.left, data.top, data.width, data.height]
}

export type Rect = readonly [
  left: number,
  top: number,
  width: number,
  height: number,
]
