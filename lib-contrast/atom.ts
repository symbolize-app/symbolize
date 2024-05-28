import type * as contrastAtomIntern from '@/atomIntern.ts'
import type * as contrastContext from '@/context.ts'
import * as contrastExpression from '@/expression.ts'

export class Atom {
  constructor(
    readonly propertyName: string | symbol,
    readonly expressionOpt: contrastExpression.ExpressionOpt<unknown>,
  ) {}

  compile(
    ctx: contrastContext.CompileContext & contrastContext.Context,
  ): contrastAtomIntern.AtomIntern {
    return ctx.contrast.atomIntern.get(
      typeof this.propertyName === 'string' ?
        this.propertyName
      : ctx.contrast.symbolCustomPropertyName.get(this.propertyName),
      contrastExpression.compile(ctx, this.expressionOpt),
    )
  }
}

export function atom(
  propertyName: string | symbol,
  value: contrastExpression.ExpressionOpt<unknown>,
): Atom {
  return new Atom(propertyName, value)
}
