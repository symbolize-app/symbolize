import * as contrastExpression from '@/expression.ts'
import * as contrastExpressionIntern from '@/expressionIntern.ts'
import type * as contrastScope from '@/scope.ts'

export function add<Scope extends contrastScope.FullScope>(
  a: contrastExpression.ExpressionOpt<number, Scope>,
  b: contrastExpression.ExpressionOpt<number, Scope>,
): contrastExpression.Expression<number, Scope> {
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
