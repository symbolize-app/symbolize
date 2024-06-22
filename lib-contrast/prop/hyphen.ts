import * as contrastAtom from '@/atom.ts'
import type * as contrastExpression from '@/expression.ts'

export const hyphen = {
  /**
   * @see https://developer.mozilla.org/en-US/docs/Web/CSS/hyphens
   */
  mode(
    value: contrastExpression.ExpressionOpt<'auto' | 'manual' | 'none'>,
  ): contrastAtom.Atom {
    return contrastAtom.atom('hyphens', value)
  },
}
