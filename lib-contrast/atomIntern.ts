import type * as contrastContext from '@/context.ts'
import type * as contrastExpressionIntern from '@/expressionIntern.ts'
import * as contrastRule from '@/rule.ts'

class AtomIntern {
  private readonly mutableRules: {
    value: contrastRule.Rule[] | null
  } = {
    value: null,
  }

  constructor(
    readonly pseudoElement: string | null,
    readonly propertyName: string,
    readonly expressionIntern: contrastExpressionIntern.ExpressionIntern,
  ) {}

  rules(ctx: contrastContext.Context): readonly contrastRule.Rule[] {
    if (!this.mutableRules.value) {
      const className = ctx.contrast.atomClassName.build()
      const code = this.expressionIntern.code(this.propertyName)
      const rule = contrastRule.rule(className, this.pseudoElement, code)
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
  expressionIntern: contrastExpressionIntern.ExpressionIntern,
): AtomIntern {
  return new AtomIntern(pseudoElement, propertyName, expressionIntern)
}
