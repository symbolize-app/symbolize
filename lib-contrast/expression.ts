import type * as compute from '@intertwine/lib-compute'

export type FullScope = 'impure' | 'pure'

export type RestrictedScope = 'pure'

export type ExpressionOpt<Value, Scope extends FullScope> =
  RestrictedScope extends Scope ?
    | Expression<Value, Scope>
    | (FullScope extends Scope ?
        ComputeExpression<Value> | MultiExpression<Value> | null
      : never)
    | (Value extends symbol ? never : Value)
  : never

export type MultiExpression<Value> = readonly ExpressionOpt<
  Value,
  FullScope
>[]

export type ComputeExpression<Value> = compute.Node<
  ExpressionOpt<Value, RestrictedScope>
>

export const expressionMarker = Symbol('expressionMarker')

export interface Expression<Value, Scope extends FullScope> {
  readonly [expressionMarker]: Scope | null

  compile(): string
  intern(): Expression<Value, Scope>
}
