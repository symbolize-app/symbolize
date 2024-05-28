import * as contrastExpression from '@/expression.ts'
import * as contrastExpressionIntern from '@/expressionIntern.ts'
import type * as contrastScope from '@/scope.ts'

export function disabled<Value, Scope extends contrastScope.FullScope>(
  value: contrastExpression.ExpressionOpt<Value, Scope>,
): contrastExpression.Expression<Value, Scope> {
  return pseudo('disabled', value)
}

export function empty<Value, Scope extends contrastScope.FullScope>(
  value: contrastExpression.ExpressionOpt<Value, Scope>,
): contrastExpression.Expression<Value, Scope> {
  return pseudo('empty', value)
}

export function hover<Value, Scope extends contrastScope.FullScope>(
  value: contrastExpression.ExpressionOpt<Value, Scope>,
): contrastExpression.Expression<Value, Scope> {
  return pseudo('hover', value)
}

function pseudo<Value, Scope extends contrastScope.FullScope>(
  name: string,
  value: contrastExpression.ExpressionOpt<Value, Scope>,
): contrastExpression.Expression<Value, Scope> {
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
