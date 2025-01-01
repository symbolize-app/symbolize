import * as stylingAtom from '@/atom.ts'
import type * as stylingExpression from '@/expression.ts'

export const pointer = {
  /**
   * @see https://developer.mozilla.org/en-US/docs/Web/CSS/pointer-events
   */
  events(
    value: stylingExpression.ExpressionOpt<'auto' | 'none'>,
  ): stylingAtom.Atom {
    return stylingAtom.atom('pointer-events', value)
  },
}
