import * as stylingAtom from '@/atom.ts'
import type * as stylingData from '@/data/index.ts'
import type * as stylingExpression from '@/expression.ts'

export const accent = {
  /**
   * @see https://developer.mozilla.org/en-US/docs/Web/CSS/accent-color
   */
  color(
    value: stylingExpression.ExpressionOpt<stylingData.Color | 'auto'>,
  ): stylingAtom.Atom {
    return stylingAtom.atom('accent-color', value)
  },
}
