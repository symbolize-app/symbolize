import * as contrastExpression from '@/expression.ts'
import * as contrastExpressionIntern from '@/expressionIntern.ts'
import type * as contrastScope from '@/scope.ts'

export function disabled<Value>(
  value: contrastExpression.ExpressionOpt<Value, contrastScope.FullScope>,
): contrastExpression.Expression<Value, contrastScope.FullScope> {
  return pseudo('disabled', value)
}

export function empty<Value>(
  value: contrastExpression.ExpressionOpt<Value, contrastScope.FullScope>,
): contrastExpression.Expression<Value, contrastScope.FullScope> {
  return pseudo('empty', value)
}

export function hover<Value>(
  value: contrastExpression.ExpressionOpt<Value, contrastScope.FullScope>,
): contrastExpression.Expression<Value, contrastScope.FullScope> {
  return pseudo('hover', value)
}

function pseudo<Value>(
  name: string,
  value: contrastExpression.ExpressionOpt<Value, contrastScope.FullScope>,
): contrastExpression.Expression<Value, contrastScope.FullScope> {
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
