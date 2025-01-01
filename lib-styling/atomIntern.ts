import type * as stylingContext from '@/context.ts'
import type * as stylingExpressionIntern from '@/expressionIntern.ts'
import * as stylingRule from '@/rule.ts'

class AtomIntern {
  private readonly mutableRules: {
    value: stylingRule.Rule[] | null
  } = {
    value: null,
  }

  constructor(
    readonly pseudoElement: string | null,
    readonly propertyName: string,
    readonly expressionIntern: stylingExpressionIntern.ExpressionIntern,
  ) {}

  rules(ctx: stylingContext.Context): readonly stylingRule.Rule[] {
    if (!this.mutableRules.value) {
      const className = ctx.styling.atomClassName.build()
      const code = this.expressionIntern.code(this.propertyName)
      const rule = stylingRule.rule(className, this.pseudoElement, code)
      this.mutableRules.value = [
        ...this.expressionIntern.extraRules(),
        rule,
      ]
    }
    return this.mutableRules.value
  }
}

export type { AtomIntern }

export function atomIntern(
  pseudoElement: string | null,
  propertyName: string,
  expressionIntern: stylingExpressionIntern.ExpressionIntern,
): AtomIntern {
  return new AtomIntern(pseudoElement, propertyName, expressionIntern)
}
