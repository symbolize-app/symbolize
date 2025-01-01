import type * as stylingContext from '@/context.ts'
import * as stylingExpressionIntern from '@/expressionIntern.ts'
import type * as stylingUtilityType from '@/utilityType.ts'

export type ExpressionOpt<Value> = Expression<Value> | Value

export const expressionMarker = Symbol('expressionMarker')
export const expressionValueMarker = Symbol('internValueMarker')

export type ExpressionInternTuple<
  Tuple extends readonly ExpressionOpt<unknown>[],
> = {
  readonly [I in keyof Tuple]: Tuple[I] extends ExpressionOpt<unknown> ?
    stylingExpressionIntern.ExpressionIntern
  : never
} & {
  readonly length: Tuple['length']
}

export interface Expression<Value> {
  compile(
    ctx: stylingContext.Context,
  ): stylingExpressionIntern.ExpressionIntern

  [expressionMarker](): unknown

  [expressionValueMarker](): Value | null
}

class ExpressionImpl<Value> implements Expression<Value> {
  constructor(
    private readonly compile_: (
      ...args: readonly unknown[]
    ) => stylingExpressionIntern.ExpressionIntern,
    private readonly args: (
      ctx: stylingContext.Context,
    ) => readonly unknown[],
  ) {}

  compile(
    ctx: stylingContext.Context,
  ): stylingExpressionIntern.ExpressionIntern {
    const internArgs = this.args(ctx)
    return ctx.styling.expressionIntern.get(this.compile_, ...internArgs)
  }

  [expressionMarker](): unknown {
    return null
  }

  [expressionValueMarker](): Value | null {
    return null
  }
}

export function expression<
  Value,
  Compile extends (
    ...args: never
  ) => stylingExpressionIntern.ExpressionIntern,
>(
  compile: Compile,
  args: (
    ctx: stylingContext.Context,
  ) => stylingUtilityType.ReadonlyParameters<Compile>,
): Expression<Value> {
  return new ExpressionImpl(compile as never, args)
}

export function isExpression<Value>(
  expression: unknown,
): expression is Expression<Value> {
  return (
    typeof expression === 'object' &&
    expression !== null &&
    expressionMarker in expression
  )
}

export function compile<Value>(
  ctx: stylingContext.Context,
  expr: ExpressionOpt<Value>,
): stylingExpressionIntern.ExpressionIntern {
  return toExpression(expr).compile(ctx)
}

export function compileAllToPure<
  Exprs extends readonly ExpressionOpt<unknown>[],
>(
  ctx: stylingContext.Context,
  ...exprs: Exprs
): {
  readonly [K in keyof Exprs]: K extends 'length' ? Exprs[K]
  : stylingExpressionIntern.PureExpressionIntern
} {
  return exprs.map((expr) => compile(ctx, expr).toPure(ctx)) as {
    readonly [K in keyof Exprs]: K extends 'length' ? Exprs[K]
    : stylingExpressionIntern.PureExpressionIntern
  }
}

export function compileScopePureExpression(
  ctx: stylingContext.Context,
  expr: ExpressionOpt<unknown>,
): stylingExpressionIntern.PureExpressionIntern {
  return stylingExpressionIntern.compileScopePureExpressionIntern(
    ctx,
    compile(ctx, expr),
  )
}

export function c<Value>(
  value: ExpressionOpt<Value>,
  ...values: readonly ExpressionOpt<Value>[]
): Expression<Value> {
  return expression(stylingExpressionIntern.compileCascade, (ctx) =>
    [value, ...values].map((item) => compile(ctx, item)),
  )
}

export function toExpression<Value>(
  expr: ExpressionOpt<Value>,
): Expression<Value> {
  if (typeof expr === 'object' && isExpression<Value>(expr)) {
    return expr
  } else {
    return expression(stylingExpressionIntern.compilePure, () => [expr])
  }
}
