import * as contrastAtom from '@/atom.ts'
import type * as contrastExpression from '@/expression.ts'
import type * as contrastScope from '@/scope.ts'

export function var_<Value>(): Var<Value> {
  return new Var()
}

export class Var<Value> {
  private readonly name = Symbol('var')

  get(): contrastExpression.Expression<
    Value,
    contrastScope.RestrictedScope
  > {
    throw new Error('not implemented')
  }

  set(
    value: contrastExpression.ExpressionOpt<
      Value,
      contrastScope.FullScope
    >,
  ): contrastAtom.Atom {
    return new contrastAtom.Atom(this.name, value)
  }
}
