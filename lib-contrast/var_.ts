import * as conveyAtom from '@/atom.ts'
import type * as conveyExpression from '@/expression.ts'

export function var_<Value>(): Var<Value> {
  return new Var()
}

export class Var<Value> {
  private readonly name = Symbol('var')

  get(): conveyExpression.Expression<
    Value,
    conveyExpression.RestrictedScope
  > {
    throw new Error('not implemented')
  }

  set(
    value: conveyExpression.ExpressionOpt<
      Value,
      conveyExpression.FullScope
    >,
  ): conveyAtom.Atom {
    return new conveyAtom.Atom(this.name, value)
  }
}
