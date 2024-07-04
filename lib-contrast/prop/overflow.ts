import * as contrastAtom from '@/atom.ts'
import type * as contrastExpression from '@/expression.ts'

type Overflow = 'auto' | 'clip' | 'hidden' | 'scroll' | 'visible'

export const overflow = {
  /**
   * @see https://developer.mozilla.org/en-US/docs/Web/CSS/overflow-x
   */
  x(value: contrastExpression.ExpressionOpt<Overflow>): contrastAtom.Atom {
    return contrastAtom.atom('overflow-x', value)
  },

  /**
   * @see https://developer.mozilla.org/en-US/docs/Web/CSS/overflow-y
   */
  y(value: contrastExpression.ExpressionOpt<Overflow>): contrastAtom.Atom {
    return contrastAtom.atom('overflow-y', value)
  },

  xy(
    value: contrastExpression.ExpressionOpt<Overflow>,
  ): contrastAtom.AtomOpt {
    return [overflow.x(value), overflow.y(value)]
  },

  /**
   * @see https://developer.mozilla.org/en-US/docs/Web/CSS/overflow-wrap
   */
  wrap(
    value: contrastExpression.ExpressionOpt<
      'anywhere' | 'break-word' | 'normal'
    >,
  ): contrastAtom.Atom {
    return contrastAtom.atom('overflow-wrap', value)
  },
}
