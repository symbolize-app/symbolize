import * as contrastAtom from '@/atom.ts'
import type * as contrastContext from '@/context.ts'
import * as contrastExpression from '@/expression.ts'
import * as contrastExpressionIntern from '@/expressionIntern.ts'

export function var_<Value>(): Var_<Value> {
  return new Var_()
}

class Var_<Value> implements contrastExpression.Expression<Value> {
  private readonly propertyName = Symbol('var')

  compile(
    ctx: contrastContext.Context,
  ): contrastExpressionIntern.ExpressionIntern {
    return ctx.contrast.expressionIntern.get(
      contrastExpressionIntern.compileCustomProperty as never,
      this.resolve(ctx),
    )
  }

  [contrastExpression.expressionMarker](): unknown {
    return null
  }

  [contrastExpression.expressionValueMarker](): Value | null {
    return null
  }

  or(
    defaultValue: contrastExpression.ExpressionOpt<Value>,
  ): contrastExpression.Expression<Exclude<Value, null>> {
    return contrastExpression.expression(
      contrastExpressionIntern.compileCustomPropertyOrDefault,
      (ctx) => [
        this.resolve(ctx),
        contrastExpression.compile(ctx, defaultValue).toPure(ctx),
      ],
    )
  }

  resolve(ctx: contrastContext.Context): string {
    return ctx.contrast.symbolCustomPropertyName.get(this.propertyName)
  }

  set(value: contrastExpression.ExpressionOpt<Value>): contrastAtom.Atom {
    return contrastAtom.atom(this.propertyName, value)
  }
}

export type { Var_ }
