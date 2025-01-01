import * as stylingAtom from '@/atom.ts'
import type * as stylingExpression from '@/expression.ts'

type Overflow = 'auto' | 'clip' | 'hidden' | 'scroll' | 'visible'

export const overflow = {
  /**
   * @see https://developer.mozilla.org/en-US/docs/Web/CSS/overflow-x
   */
  x(value: stylingExpression.ExpressionOpt<Overflow>): stylingAtom.Atom {
    return stylingAtom.atom('overflow-x', value)
  },

  /**
   * @see https://developer.mozilla.org/en-US/docs/Web/CSS/overflow-y
   */
  y(value: stylingExpression.ExpressionOpt<Overflow>): stylingAtom.Atom {
    return stylingAtom.atom('overflow-y', value)
  },

  xy(
    value: stylingExpression.ExpressionOpt<Overflow>,
  ): stylingAtom.AtomOpt {
    return [overflow.x(value), overflow.y(value)]
  },

  /**
   * @see https://developer.mozilla.org/en-US/docs/Web/CSS/overflow-wrap
   */
  wrap(
    value: stylingExpression.ExpressionOpt<
      'anywhere' | 'break-word' | 'normal'
    >,
  ): stylingAtom.Atom {
    return stylingAtom.atom('overflow-wrap', value)
  },
}
