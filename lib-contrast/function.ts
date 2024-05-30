import * as contrastExpression from '@/expression.ts'
import * as contrastExpressionIntern from '@/expressionIntern.ts'

export function add(
  a: contrastExpression.ExpressionOpt<number>,
  b: contrastExpression.ExpressionOpt<number>,
): contrastExpression.Expression<number> {
  return contrastExpression.expression(compileAdd, (ctx) => [
    contrastExpression.compileToPure(ctx, a),
    contrastExpression.compileToPure(ctx, b),
  ])
}

function compileAdd(
  a: contrastExpressionIntern.PureExpressionIntern,
  b: contrastExpressionIntern.PureExpressionIntern,
): contrastExpressionIntern.PureExpressionIntern {
  return contrastExpressionIntern.compilePure(
    `calc(${a.value}+${b.value})`,
    a,
    b,
  )
}
