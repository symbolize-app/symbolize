import * as contrastAtom from '@/atom.ts'
import type * as contrastData from '@/data/index.ts'
import type * as contrastExpression from '@/expression.ts'

export const accent = {
  /**
   * @see https://developer.mozilla.org/en-US/docs/Web/CSS/accent-color
   */
  color(
    value: contrastExpression.ExpressionOpt<contrastData.Color | 'auto'>,
  ): contrastAtom.Atom {
    return contrastAtom.atom('accent-color', value)
  },
}
