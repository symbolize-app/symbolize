import type * as contrastContext from '@/context.ts'
import * as contrastExpressionIntern from '@/expressionIntern.ts'
import type * as contrastScope from '@/scope.ts'
import * as compute from '@intertwine/lib-compute'

export type ExpressionOpt<Value, Scope extends contrastScope.FullScope> =
  contrastScope.RestrictedScope extends Scope ?
    | Expression<Value, Scope>
    | (contrastScope.FullScope extends Scope ?
        ComputeExpression<Value> | MultiExpression<Value> | null
      : never)
    | (Value extends symbol ? never : Value)
  : never

export type MultiExpression<Value> = readonly ExpressionOpt<
  Value,
  contrastScope.FullScope
>[]

export type ComputeExpression<Value> = compute.Node<
  ExpressionOpt<Value, contrastScope.RestrictedScope>
>

export const expressionMarker = Symbol('expressionMarker')
export const expressionValueMarker = Symbol('internValueMarker')
export const expressionScopeMarker = Symbol('internScopeMarker')

export interface Expression<Value, Scope extends contrastScope.FullScope> {
  readonly [expressionMarker]: null

  compile(
    ctx: contrastContext.Context,
  ): contrastExpressionIntern.ExpressionIntern

  [expressionScopeMarker](scope: Scope): Scope | null

  [expressionValueMarker](): Value | null
}

export type ExpressionInternTuple<
  Tuple extends readonly ExpressionOpt<unknown, Scope>[],
  Scope extends contrastScope.FullScope,
> = {
  readonly [I in keyof Tuple]: Tuple[I] extends (
    ExpressionOpt<unknown, Scope>
  ) ?
    contrastExpressionIntern.ExpressionIntern
  : never
} & {
  readonly length: Tuple['length']
}

export class ExpressionImpl<
  Value,
  Scope extends contrastScope.FullScope,
  InternCompile extends (
    ...args: never
  ) => contrastExpressionIntern.ExpressionIntern,
> implements Expression<Value, Scope>
{
  readonly [expressionMarker] = null

  constructor(
    private readonly internCompile: InternCompile,
    private readonly args: (
      ctx: contrastContext.Context,
    ) => Readonly<ReadonlyParameters<InternCompile>>,
  ) {}

  compile(
    ctx: contrastContext.Context,
  ): contrastExpressionIntern.ExpressionIntern {
    const internArgs = this.args(ctx)
    return ctx.contrast.expressionIntern.get(
      this.internCompile as never,
      ...internArgs,
    )
  }

  [expressionScopeMarker](): Scope | null {
    return null
  }

  [expressionValueMarker](): Value | null {
    return null
  }
}

type ReadonlyParameters<T extends (...args: never) => unknown> =
  T extends (...args: readonly [...infer P]) => unknown ? P : never

export function expression<
  Value,
  Scope extends contrastScope.FullScope,
  InternCompile extends (
    ...args: never
  ) => contrastExpressionIntern.ExpressionIntern,
>(
  internCompile: InternCompile,
  args: (
    ctx: contrastContext.Context,
  ) => Readonly<ReadonlyParameters<InternCompile>>,
): ExpressionImpl<Value, Scope, InternCompile> {
  return new ExpressionImpl(internCompile, args)
}

export function isExpression<Value, Scope extends contrastScope.FullScope>(
  expression: unknown,
): expression is Expression<Value, Scope> {
  return (
    typeof expression === 'object' &&
    expression !== null &&
    expressionMarker in expression
  )
}

export function compile<Value, Scope extends contrastScope.FullScope>(
  ctx: contrastContext.Context,
  expr: ExpressionOpt<Value, Scope>,
): contrastExpressionIntern.ExpressionIntern {
  return toExpression(expr).compile(ctx)
}

export function compileToPure<
  Value,
  Scope extends contrastScope.FullScope,
>(
  ctx: contrastContext.Context,
  expr: ExpressionOpt<Value, Scope>,
): contrastExpressionIntern.PureExpressionIntern {
  return compile(ctx, expr).toPure(ctx)
}

export function toExpression<Value, Scope extends contrastScope.FullScope>(
  expr: ExpressionOpt<Value, Scope>,
): Expression<Value, Scope> {
  if (typeof expr === 'object') {
    if (expr === null) {
      return expression(
        contrastExpressionIntern.compileMulti,
        () => [] as const,
      ) as never
    } else if (Array.isArray(expr)) {
      return expression(contrastExpressionIntern.compileMulti, (ctx) =>
        expr.map((item) => compile(ctx, item)),
      ) as never
    } else if (compute.isNode(expr)) {
      throw new Error()
    } else if (isExpression<Value, Scope>(expr)) {
      return expr
    }
  }
  return expression(
    contrastExpressionIntern.compilePure,
    () => [expr] as const,
  ) as never
}
