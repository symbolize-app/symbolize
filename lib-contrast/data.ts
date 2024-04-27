import * as conveyExpression from '@/expression.ts'

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

const colorMarker = Symbol('colorMarker')

export type Color = typeof colorMarker

export function rgb<Scope extends conveyExpression.FullScope>(
  r: conveyExpression.ExpressionOpt<number, Scope>,
  g: conveyExpression.ExpressionOpt<number, Scope>,
  b: conveyExpression.ExpressionOpt<number, Scope>,
): ColorRgbExpression<Scope> {
  return new ColorRgbExpression(r, g, b)
}

export class ColorRgbExpression<Scope extends conveyExpression.FullScope>
  implements conveyExpression.Expression<Color, Scope>
{
  readonly [conveyExpression.expressionMarker] = null

  constructor(
    private readonly r: conveyExpression.ExpressionOpt<number, Scope>,
    private readonly g: conveyExpression.ExpressionOpt<number, Scope>,
    private readonly b: conveyExpression.ExpressionOpt<number, Scope>,
  ) {}

  compile(): string {
    throw new Error(
      `Method not implemented. ${typeof this.r} ${typeof this.g} ${typeof this.b}`,
    )
  }

  intern(): conveyExpression.Expression<typeof colorMarker, Scope> {
    throw new Error('Method not implemented.')
  }
}
