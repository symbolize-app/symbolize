import type * as contrastContext from '@/context.ts'
import * as contrastExpressionIntern from '@/expressionIntern.ts'
import type * as contrastUtilityType from '@/utilityType.ts'

export type ExpressionOpt<Value> = Expression<Value> | Value

export const expressionMarker = Symbol('expressionMarker')
export const expressionValueMarker = Symbol('internValueMarker')

export type ExpressionInternTuple<
  Tuple extends readonly ExpressionOpt<unknown>[],
> = {
  readonly [I in keyof Tuple]: Tuple[I] extends ExpressionOpt<unknown> ?
    contrastExpressionIntern.ExpressionIntern
  : never
} & {
  readonly length: Tuple['length']
}

export interface Expression<Value> {
  compile(
    ctx: contrastContext.Context,
  ): contrastExpressionIntern.ExpressionIntern

  [expressionMarker](): unknown

  [expressionValueMarker](): Value | null
}

class ExpressionImpl<Value> implements Expression<Value> {
  constructor(
    private readonly compile_: (
      ...args: readonly unknown[]
    ) => contrastExpressionIntern.ExpressionIntern,
    private readonly args: (
      ctx: contrastContext.Context,
    ) => readonly unknown[],
  ) {}

  compile(
    ctx: contrastContext.Context,
  ): contrastExpressionIntern.ExpressionIntern {
    const internArgs = this.args(ctx)
    return ctx.contrast.expressionIntern.get(this.compile_, ...internArgs)
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
  ) => contrastExpressionIntern.ExpressionIntern,
>(
  compile: Compile,
  args: (
    ctx: contrastContext.Context,
  ) => contrastUtilityType.ReadonlyParameters<Compile>,
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
  ctx: contrastContext.Context,
  expr: ExpressionOpt<Value>,
): contrastExpressionIntern.ExpressionIntern {
  return toExpression(expr).compile(ctx)
}

export function compileAllToPure<
  Exprs extends readonly ExpressionOpt<unknown>[],
>(
  ctx: contrastContext.Context,
  ...exprs: Exprs
): {
  readonly [K in keyof Exprs]: K extends 'length' ? Exprs[K]
  : contrastExpressionIntern.PureExpressionIntern
} {
  return exprs.map((expr) => compile(ctx, expr).toPure(ctx)) as {
    readonly [K in keyof Exprs]: K extends 'length' ? Exprs[K]
    : contrastExpressionIntern.PureExpressionIntern
  }
}

export function compileScopePureExpression(
  ctx: contrastContext.Context,
  expr: ExpressionOpt<unknown>,
): contrastExpressionIntern.PureExpressionIntern {
  return contrastExpressionIntern.compileScopePureExpressionIntern(
    ctx,
    compile(ctx, expr),
  )
}

export function c<Value>(
  value: ExpressionOpt<Value>,
  ...values: readonly ExpressionOpt<Value>[]
): Expression<Value> {
  return expression(contrastExpressionIntern.compileCascade, (ctx) =>
    [value, ...values].map((item) => compile(ctx, item)),
  )
}

export function toExpression<Value>(
  expr: ExpressionOpt<Value>,
): Expression<Value> {
  if (typeof expr === 'object' && isExpression<Value>(expr)) {
    return expr
  } else {
    return expression(contrastExpressionIntern.compilePure, () => [expr])
  }
}
