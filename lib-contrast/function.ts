import type * as contrastData from '@/data/index.ts'
import * as contrastExpression from '@/expression.ts'
import * as contrastExpressionIntern from '@/expressionIntern.ts'

export function add<
  Value extends contrastData.Length | contrastData.Pct | number,
>(
  initial: contrastExpression.ExpressionOpt<Value>,
  ...extra: readonly contrastExpression.ExpressionOpt<Value>[]
): contrastExpression.Expression<Value> {
  return contrastExpression.expression(compileCalc, (ctx) => [
    '+',
    ...contrastExpression.compileAllToPure(ctx, initial, ...extra),
  ])
}

export function sub<
  Value extends contrastData.Length | contrastData.Pct | number,
>(
  initial: contrastExpression.ExpressionOpt<Value>,
  ...extra: readonly contrastExpression.ExpressionOpt<Value>[]
): contrastExpression.Expression<Value> {
  return contrastExpression.expression(compileCalc, (ctx) => [
    '-',
    ...contrastExpression.compileAllToPure(ctx, initial, ...extra),
  ])
}

export function concat(
  ...values: readonly contrastExpression.ExpressionOpt<contrastData.String_>[]
): contrastExpression.Expression<contrastData.String_> {
  return contrastExpression.expression(
    contrastExpressionIntern.compilePureSpaceSeparator,
    (ctx) => contrastExpression.compileAllToPure(ctx, ...values),
  )
}

function compileCalc(
  operator: string,
  ...dependencies: readonly contrastExpressionIntern.PureExpressionIntern[]
): contrastExpressionIntern.PureExpressionIntern {
  return contrastExpressionIntern.compilePureFunctionSeparator(
    'calc',
    ` ${operator} `,
    ...dependencies,
  )
}
