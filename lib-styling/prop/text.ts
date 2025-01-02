import * as stylingAtom from '@/atom.ts'
import type * as stylingExpression from '@/expression.ts'

export const text = {
  /**
   * @see https://developer.mozilla.org/en-US/docs/Web/CSS/text-wrap
   */
  wrap(
    value: stylingExpression.ExpressionOpt<
      'balance' | 'nowrap' | 'pretty' | 'stable' | 'wrap'
    >,
  ): stylingAtom.Atom {
    return stylingAtom.atom('text-wrap', value)
  },
}
