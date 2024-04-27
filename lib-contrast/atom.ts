import type * as conveyExpression from '@/expression.ts'

export class Atom<Value = unknown> {
  constructor(
    readonly name: string | symbol,
    readonly value: conveyExpression.ExpressionOpt<
      Value,
      conveyExpression.FullScope
    >,
  ) {}
}
