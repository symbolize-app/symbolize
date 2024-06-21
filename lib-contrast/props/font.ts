import * as contrastAtom from '@/atom.ts'
import type * as contrastExpression from '@/expression.ts'

export const font = {
  variant: {
    ligatures(
      value: contrastExpression.ExpressionOpt<
        | 'common-ligatures'
        | 'contextual'
        | 'discretionary-ligatures'
        | 'historical-ligatures'
        | 'no-common-ligatures'
        | 'no-contextual'
        | 'no-discretionary-ligatures'
        | 'no-historical-ligatures'
        | 'none'
        | 'normal'
      >,
    ): contrastAtom.Atom {
      return contrastAtom.atom('font-variant-ligatures', value)
    },
  },
}
