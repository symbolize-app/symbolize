import * as stylingAtom from '@/atom.ts'
import type * as stylingExpression from '@/expression.ts'

export const hyphen = {
  /**
   * @see https://developer.mozilla.org/en-US/docs/Web/CSS/hyphens
   */
  mode(
    value: stylingExpression.ExpressionOpt<'auto' | 'manual' | 'none'>,
  ): stylingAtom.Atom {
    return stylingAtom.atom('hyphens', value)
  },
}
