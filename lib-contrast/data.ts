import * as contrastExpression from '@/expression.ts'
import * as contrastExpressionIntern from '@/expressionIntern.ts'
import type * as contrastScope from '@/scope.ts'

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

export function rgb<Scope extends contrastScope.FullScope>(
  r: contrastExpression.ExpressionOpt<number, Scope>,
  g: contrastExpression.ExpressionOpt<number, Scope>,
  b: contrastExpression.ExpressionOpt<number, Scope>,
): contrastExpression.Expression<Color, Scope> {
  return contrastExpression.expression(compileColorRgb, (ctx) => [
    contrastExpression.compileToPure(ctx, r),
    contrastExpression.compileToPure(ctx, g),
    contrastExpression.compileToPure(ctx, b),
  ])
}

function compileColorRgb(
  r: contrastExpressionIntern.PureExpressionIntern,
  g: contrastExpressionIntern.PureExpressionIntern,
  b: contrastExpressionIntern.PureExpressionIntern,
): contrastExpressionIntern.PureExpressionIntern {
  return contrastExpressionIntern.compilePure(
    `rgb(${r.value},${g.value},${b.value})`,
    r,
    g,
    b,
  )
}

export type SvgPaint = Color | 'context-fill' | 'context-stroke'
