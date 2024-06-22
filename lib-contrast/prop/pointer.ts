import * as contrastAtom from '@/atom.ts'
import type * as contrastExpression from '@/expression.ts'

export const pointer = {
  /**
   * @see https://developer.mozilla.org/en-US/docs/Web/CSS/pointer-events
   */
  events(
    value: contrastExpression.ExpressionOpt<'auto' | 'none'>,
  ): contrastAtom.Atom {
    return contrastAtom.atom('pointer-events', value)
  },
}
