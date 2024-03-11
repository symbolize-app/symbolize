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

export function cm(value: number): Length<'cm'> {
  return new Length(value, 'cm')
}

export function em(value: number): Length<'em'> {
  return new Length(value, 'em')
}

export function ex(value: number): Length<'ex'> {
  return new Length(value, 'ex')
}

export function in_(value: number): Length<'in'> {
  return new Length(value, 'in')
}

export function mm(value: number): Length<'mm'> {
  return new Length(value, 'mm')
}

export function pc(value: number): Length<'pc'> {
  return new Length(value, 'pc')
}

export function percent(value: number): Length<'%'> {
  return new Length(value, '%')
}

export function pt(value: number): Length<'pt'> {
  return new Length(value, 'pt')
}

export function px(value: number): Length<'px'> {
  return new Length(value, 'px')
}

export class Length<Unit extends string> {
  constructor(
    readonly value: number,
    readonly unit: Unit,
  ) {}

  toString(): string {
    if (this.unit) {
      return `${this.value}${this.unit}`
    } else {
      return `${this.value}`
    }
  }
}

export type SvgLengthOpt = SvgLength | SvgLength['value']

export type SvgLength = Length<
  '%' | 'cm' | 'em' | 'ex' | 'in' | 'mm' | 'pc' | 'pt' | 'px'
>

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
