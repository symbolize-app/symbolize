import * as contrastExpression from '@/expression.ts'
import * as contrastExpressionIntern from '@/expressionIntern.ts'

export function disabled<Value>(
  value: contrastExpression.ExpressionOpt<Value>,
): contrastExpression.Expression<Value> {
  return pseudo('disabled', value)
}

export function empty<Value>(
  value: contrastExpression.ExpressionOpt<Value>,
): contrastExpression.Expression<Value> {
  return pseudo('empty', value)
}

export function hover<Value>(
  value: contrastExpression.ExpressionOpt<Value>,
): contrastExpression.Expression<Value> {
  return pseudo('hover', value)
}

function pseudo<Value>(
  name: string,
  value: contrastExpression.ExpressionOpt<Value>,
): contrastExpression.Expression<Value> {
  return contrastExpression.expression(compilePseudo, (ctx) => [
    name,
    contrastExpression.compile(ctx, value),
  ])
}

function compilePseudo(
  name: string,
  value: contrastExpressionIntern.ExpressionIntern,
): contrastExpressionIntern.ExpressionIntern {
  return contrastExpressionIntern.compileScope(`&:where(:${name})`, value)
}
