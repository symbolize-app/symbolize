import * as contrastExpression from '@/expression.ts'
import * as contrastExpressionIntern from '@/expressionIntern.ts'

export function disabled<Value>(
  value: contrastExpression.ExpressionOpt<Value>,
): contrastExpression.Expression<Value> {
  return pseudoState('disabled', value)
}

export function empty<Value>(
  value: contrastExpression.ExpressionOpt<Value>,
): contrastExpression.Expression<Value> {
  return pseudoState('empty', value)
}

export function hover<Value>(
  value: contrastExpression.ExpressionOpt<Value>,
): contrastExpression.Expression<Value> {
  return pseudoState('hover', value)
}

function pseudoState<Value>(
  name: string,
  value: contrastExpression.ExpressionOpt<Value>,
): contrastExpression.Expression<Value> {
  return contrastExpression.expression(compilePseudoState, (ctx) => [
    name,
    contrastExpression.compile(ctx, value),
  ])
}

function compilePseudoState(
  name: string,
  value: contrastExpressionIntern.ExpressionIntern,
): contrastExpressionIntern.ExpressionIntern {
  return contrastExpressionIntern.compileScope(`&:where(:${name})`, value)
}
