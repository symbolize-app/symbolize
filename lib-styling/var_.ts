import * as stylingAtom from '@/atom.ts'
import type * as stylingContext from '@/context.ts'
import * as stylingExpression from '@/expression.ts'
import * as stylingExpressionIntern from '@/expressionIntern.ts'

export function var_<Value>(): Var_<Value> {
  return new Var_()
}

class Var_<Value> implements stylingExpression.Expression<Value> {
  private readonly propertyName = Symbol('var')

  compile(
    ctx: stylingContext.Context,
  ): stylingExpressionIntern.ExpressionIntern {
    return ctx.styling.expressionIntern.get(
      stylingExpressionIntern.compileCustomProperty as never,
      this.resolve(ctx),
    )
  }

  or(
    defaultValue: stylingExpression.ExpressionOpt<Value>,
  ): stylingExpression.Expression<Exclude<Value, null>> {
    return stylingExpression.expression(
      stylingExpressionIntern.compileCustomPropertyOrDefault,
      (ctx) => [
        this.resolve(ctx),
        stylingExpression.compile(ctx, defaultValue).toPure(ctx),
      ],
    )
  }

  resolve(ctx: stylingContext.Context): string {
    return ctx.styling.symbolCustomPropertyName.get(this.propertyName)
  }

  set(value: stylingExpression.ExpressionOpt<Value>): stylingAtom.Atom {
    return stylingAtom.atom(this.propertyName, value)
  }

  [stylingExpression.expressionMarker](): unknown {
    return null
  }

  [stylingExpression.expressionValueMarker](): Value | null {
    return null
  }
}

export type { Var_ }
