import type * as contrastData from '@/data/index.ts'
import * as contrastExpression from '@/expression.ts'
import * as contrastExpressionIntern from '@/expressionIntern.ts'

export function add<
  ValueInitial extends contrastData.Length | contrastData.Pct | number,
  ValueExtra extends contrastData.Length | contrastData.Pct | number,
>(
  initial: contrastExpression.ExpressionOpt<ValueInitial>,
  ...extra: readonly contrastExpression.ExpressionOpt<ValueExtra>[]
): contrastExpression.Expression<ValueExtra | ValueInitial> {
  return contrastExpression.expression(compileCalc, (ctx) => [
    '+',
    ...contrastExpression.compileAllToPure(ctx, initial, ...extra),
  ])
}

export function clamp<
  ValueInitial extends contrastData.Length | contrastData.Pct | number,
  ValueExtra extends contrastData.Length | contrastData.Pct | number,
>(
  lower: contrastExpression.ExpressionOpt<ValueExtra>,
  initial: contrastExpression.ExpressionOpt<ValueInitial>,
  upper: contrastExpression.ExpressionOpt<ValueExtra>,
): contrastExpression.Expression<ValueExtra | ValueInitial> {
  return contrastExpression.expression(
    contrastExpressionIntern.compilePureFunction,
    (ctx) => [
      'clamp',
      ...contrastExpression.compileAllToPure(ctx, lower, initial, upper),
    ],
  )
}

export function div<
  ValueInitial extends contrastData.Length | contrastData.Pct | number,
>(
  initial: contrastExpression.ExpressionOpt<ValueInitial>,
  ...extra: readonly number[]
): contrastExpression.Expression<ValueInitial> {
  return contrastExpression.expression(compileCalc, (ctx) => [
    '/',
    ...contrastExpression.compileAllToPure(ctx, initial, ...extra),
  ])
}

export function max<
  ValueInitial extends contrastData.Length | contrastData.Pct | number,
  ValueExtra extends contrastData.Length | contrastData.Pct | number,
>(
  initial: contrastExpression.ExpressionOpt<ValueInitial>,
  ...extra: readonly contrastExpression.ExpressionOpt<ValueExtra>[]
): contrastExpression.Expression<ValueExtra | ValueInitial> {
  return contrastExpression.expression(
    contrastExpressionIntern.compilePureFunction,
    (ctx) => [
      'max',
      ...contrastExpression.compileAllToPure(ctx, initial, ...extra),
    ],
  )
}

export function min<
  ValueInitial extends contrastData.Length | contrastData.Pct | number,
  ValueExtra extends contrastData.Length | contrastData.Pct | number,
>(
  initial: contrastExpression.ExpressionOpt<ValueInitial>,
  ...extra: readonly contrastExpression.ExpressionOpt<ValueExtra>[]
): contrastExpression.Expression<ValueExtra | ValueInitial> {
  return contrastExpression.expression(
    contrastExpressionIntern.compilePureFunction,
    (ctx) => [
      'min',
      ...contrastExpression.compileAllToPure(ctx, initial, ...extra),
    ],
  )
}

export function mul<
  ValueInitial extends contrastData.Length | contrastData.Pct | number,
>(
  initial: contrastExpression.ExpressionOpt<ValueInitial>,
  ...extra: readonly number[]
): contrastExpression.Expression<ValueInitial> {
  return contrastExpression.expression(compileCalc, (ctx) => [
    '*',
    ...contrastExpression.compileAllToPure(ctx, initial, ...extra),
  ])
}

export function sub<
  ValueInitial extends contrastData.Length | contrastData.Pct | number,
  ValueExtra extends contrastData.Length | contrastData.Pct | number,
>(
  initial: contrastExpression.ExpressionOpt<ValueInitial>,
  ...extra: readonly contrastExpression.ExpressionOpt<ValueExtra>[]
): contrastExpression.Expression<ValueExtra | ValueInitial> {
  return contrastExpression.expression(compileCalc, (ctx) => [
    '-',
    ...contrastExpression.compileAllToPure(ctx, initial, ...extra),
  ])
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
