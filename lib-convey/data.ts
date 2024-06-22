import type * as contrast from '@intertwine/lib-contrast'

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

const svgPreserveAspectRatioMarker = Symbol('svgPreserveAspectRatioMarker')

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

  [svgPreserveAspectRatioMarker](): unknown {
    return null
  }

  toString(): string {
    return `${this.align} ${this.mode}`
  }
}

export type SvgLengthPctOpt = contrast.Pct | SvgLength | SvgLength['value']

export type SvgLengthUnit =
  | 'cm'
  | 'em'
  | 'ex'
  | 'in'
  | 'mm'
  | 'pc'
  | 'pt'
  | 'px'

export type SvgLength = contrast.Length<SvgLengthUnit>

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
