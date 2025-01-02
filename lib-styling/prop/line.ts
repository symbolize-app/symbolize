import * as stylingAtom from '@/atom.ts'
import type * as stylingData from '@/data/index.ts'
import type * as stylingExpression from '@/expression.ts'

export const line = {
  /**
   * @see https://developer.mozilla.org/en-US/docs/Web/CSS/line-height
   */
  height(
    value: stylingExpression.ExpressionOpt<
      stylingData.Length | stylingData.Pct | number | 'normal'
    >,
  ): stylingAtom.Atom {
    return stylingAtom.atom('line-height', value)
  },
}
