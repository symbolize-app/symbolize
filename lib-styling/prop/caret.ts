import * as stylingAtom from '@/atom.ts'
import type * as stylingData from '@/data/index.ts'
import type * as stylingExpression from '@/expression.ts'

export const caret = {
  /**
   * @see https://developer.mozilla.org/en-US/docs/Web/CSS/caret-color
   */
  color(
    value: stylingExpression.ExpressionOpt<stylingData.Color | 'auto'>,
  ): stylingAtom.Atom {
    return stylingAtom.atom('caret-color', value)
  },
}
