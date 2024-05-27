import * as contrastAtom from '@/atom.ts'
import * as contrastExpression from '@/expression.ts'
import * as contrastExpressionIntern from '@/expressionIntern.ts'
import type * as contrastScope from '@/scope.ts'

export function customProperty<Value>(): CustomProperty<Value> {
  return new CustomProperty()
}

export class CustomProperty<Value> {
  private readonly propertyName = Symbol('customProperty')

  get(): contrastExpression.Expression<
    Value,
    contrastScope.RestrictedScope
  > {
    return contrastExpression.expression(compileCustomProperty, (ctx) => [
      ctx.contrast.symbolCustomPropertyName.get(this.propertyName),
    ])
  }

  getOr<Scope extends contrastScope.FullScope>(
    defaultValue: contrastExpression.ExpressionOpt<Value, Scope>,
  ): contrastExpression.Expression<Value, contrastScope.RestrictedScope> {
    return contrastExpression.expression(
      compileCustomPropertyOrDefault,
      (ctx) => [
        ctx.contrast.symbolCustomPropertyName.get(this.propertyName),
        contrastExpression.compileToPure(ctx, defaultValue),
      ],
    )
  }

  set(
    value: contrastExpression.ExpressionOpt<
      Value,
      contrastScope.FullScope
    >,
  ): contrastAtom.Atom {
    return contrastAtom.atom(this.propertyName, value)
  }
}

function compileCustomProperty(
  propertyName: string,
): contrastExpressionIntern.PureExpressionIntern {
  return contrastExpressionIntern.compilePure(`var(${propertyName})`)
}

function compileCustomPropertyOrDefault(
  propertyName: string,
  defaultValue: contrastExpressionIntern.PureExpressionIntern,
): contrastExpressionIntern.PureExpressionIntern {
  return contrastExpressionIntern.compilePure(
    `var(${propertyName},${defaultValue.value})`,
    defaultValue,
  )
}
