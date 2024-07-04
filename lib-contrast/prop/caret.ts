import * as contrastAtom from '@/atom.ts'
import type * as contrastData from '@/data/index.ts'
import type * as contrastExpression from '@/expression.ts'

export const caret = {
  /**
   * @see https://developer.mozilla.org/en-US/docs/Web/CSS/caret-color
   */
  color(
    value: contrastExpression.ExpressionOpt<contrastData.Color | 'auto'>,
  ): contrastAtom.Atom {
    return contrastAtom.atom('caret-color', value)
  },
}
