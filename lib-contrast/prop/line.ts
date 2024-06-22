import * as contrastAtom from '@/atom.ts'
import type * as contrastData from '@/data/index.ts'
import type * as contrastExpression from '@/expression.ts'

export const line = {
  /**
   * @see https://developer.mozilla.org/en-US/docs/Web/CSS/line-height
   */
  height(
    value: contrastExpression.ExpressionOpt<
      contrastData.Length | contrastData.Pct | number | 'normal'
    >,
  ): contrastAtom.Atom {
    return contrastAtom.atom('line-height', value)
  },
}
