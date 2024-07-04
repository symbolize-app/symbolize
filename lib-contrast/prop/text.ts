import * as contrastAtom from '@/atom.ts'
import type * as contrastExpression from '@/expression.ts'

export const text = {
  /**
   * @see https://developer.mozilla.org/en-US/docs/Web/CSS/text-wrap
   */
  wrap(
    value: contrastExpression.ExpressionOpt<
      'balance' | 'nowrap' | 'pretty' | 'stable' | 'wrap'
    >,
  ): contrastAtom.Atom {
    return contrastAtom.atom('text-wrap', value)
  },
}
