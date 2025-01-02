import type * as stylingData from '@/data/index.ts'
import * as stylingExpression from '@/expression.ts'
import * as stylingExpressionIntern from '@/expressionIntern.ts'

export function add<
  ValueInitial extends stylingData.Length | stylingData.Pct | number,
  ValueExtra extends stylingData.Length | stylingData.Pct | number,
>(
  initial: stylingExpression.ExpressionOpt<ValueInitial>,
  ...extra: readonly stylingExpression.ExpressionOpt<ValueExtra>[]
): stylingExpression.Expression<ValueExtra | ValueInitial> {
  return stylingExpression.expression(compileCalc, (ctx) => [
    '+',
    ...stylingExpression.compileAllToPure(ctx, initial, ...extra),
  ])
}

export function clamp<
  ValueInitial extends stylingData.Length | stylingData.Pct | number,
  ValueExtra extends stylingData.Length | stylingData.Pct | number,
>(
  lower: stylingExpression.ExpressionOpt<ValueExtra>,
  initial: stylingExpression.ExpressionOpt<ValueInitial>,
  upper: stylingExpression.ExpressionOpt<ValueExtra>,
): stylingExpression.Expression<ValueExtra | ValueInitial> {
  return stylingExpression.expression(
    stylingExpressionIntern.compilePureFunction,
    (ctx) => [
      'clamp',
      ...stylingExpression.compileAllToPure(ctx, lower, initial, upper),
    ],
  )
}

export function div<
  ValueInitial extends stylingData.Length | stylingData.Pct | number,
>(
  initial: stylingExpression.ExpressionOpt<ValueInitial>,
  ...extra: readonly number[]
): stylingExpression.Expression<ValueInitial> {
  return stylingExpression.expression(compileCalc, (ctx) => [
    '/',
    ...stylingExpression.compileAllToPure(ctx, initial, ...extra),
  ])
}

export function max<
  ValueInitial extends stylingData.Length | stylingData.Pct | number,
  ValueExtra extends stylingData.Length | stylingData.Pct | number,
>(
  initial: stylingExpression.ExpressionOpt<ValueInitial>,
  ...extra: readonly stylingExpression.ExpressionOpt<ValueExtra>[]
): stylingExpression.Expression<ValueExtra | ValueInitial> {
  return stylingExpression.expression(
    stylingExpressionIntern.compilePureFunction,
    (ctx) => [
      'max',
      ...stylingExpression.compileAllToPure(ctx, initial, ...extra),
    ],
  )
}

export function min<
  ValueInitial extends stylingData.Length | stylingData.Pct | number,
  ValueExtra extends stylingData.Length | stylingData.Pct | number,
>(
  initial: stylingExpression.ExpressionOpt<ValueInitial>,
  ...extra: readonly stylingExpression.ExpressionOpt<ValueExtra>[]
): stylingExpression.Expression<ValueExtra | ValueInitial> {
  return stylingExpression.expression(
    stylingExpressionIntern.compilePureFunction,
    (ctx) => [
      'min',
      ...stylingExpression.compileAllToPure(ctx, initial, ...extra),
    ],
  )
}

export function mul<
  ValueInitial extends stylingData.Length | stylingData.Pct | number,
>(
  initial: stylingExpression.ExpressionOpt<ValueInitial>,
  ...extra: readonly number[]
): stylingExpression.Expression<ValueInitial> {
  return stylingExpression.expression(compileCalc, (ctx) => [
    '*',
    ...stylingExpression.compileAllToPure(ctx, initial, ...extra),
  ])
}

export function sub<
  ValueInitial extends stylingData.Length | stylingData.Pct | number,
  ValueExtra extends stylingData.Length | stylingData.Pct | number,
>(
  initial: stylingExpression.ExpressionOpt<ValueInitial>,
  ...extra: readonly stylingExpression.ExpressionOpt<ValueExtra>[]
): stylingExpression.Expression<ValueExtra | ValueInitial> {
  return stylingExpression.expression(compileCalc, (ctx) => [
    '-',
    ...stylingExpression.compileAllToPure(ctx, initial, ...extra),
  ])
}

function compileCalc(
  operator: string,
  ...dependencies: readonly stylingExpressionIntern.PureExpressionIntern[]
): stylingExpressionIntern.PureExpressionIntern {
  return stylingExpressionIntern.compilePureFunctionSeparator(
    'calc',
    ` ${operator} `,
    ...dependencies,
  )
}
