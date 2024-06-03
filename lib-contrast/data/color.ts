import * as contrastExpression from '@/expression.ts'
import * as contrastExpressionIntern from '@/expressionIntern.ts'

const colorMarker = Symbol('colorMarker')

export type Color = typeof colorMarker

export function rgb(
  r: contrastExpression.ExpressionOpt<number>,
  g: contrastExpression.ExpressionOpt<number>,
  b: contrastExpression.ExpressionOpt<number>,
): contrastExpression.Expression<Color> {
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
