import * as contrastAtom from '@/atom.ts'
import type * as contrastContext from '@/context.ts'
import * as contrastExpression from '@/expression.ts'
import * as contrastExpressionIntern from '@/expressionIntern.ts'
import type * as contrastScope from '@/scope.ts'

export function var_<Value>(): Var_<Value> {
  return new Var_()
}

export class Var_<Value> {
  private readonly propertyName = Symbol('customProperty')

  get(): contrastExpression.Expression<Value> {
    return contrastExpression.expression(
      contrastExpressionIntern.compileCustomProperty,
      (ctx) => [
        ctx.contrast.symbolCustomPropertyName.get(this.propertyName),
      ],
    )
  }

  getOr<Scope extends contrastScope.FullScope>(
    defaultValue: contrastExpression.ExpressionOpt<Value, Scope>,
  ): contrastExpression.Expression<Exclude<Value, null>, Scope> {
    return contrastExpression.expression(
      contrastExpressionIntern.compileCustomPropertyOrDefault,
      (ctx) => [
        ctx.contrast.symbolCustomPropertyName.get(this.propertyName),
        contrastExpression.compileToPure(ctx, defaultValue),
      ],
    )
  }

  resolveCustomPropertyName(ctx: contrastContext.Context): string {
    return ctx.contrast.symbolCustomPropertyName.get(this.propertyName)
  }

  set(value: contrastExpression.ExpressionOpt<Value>): contrastAtom.Atom {
    return contrastAtom.atom(this.propertyName, value)
  }
}
