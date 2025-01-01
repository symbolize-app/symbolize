import * as stylingAtom from '@/atom.ts'
import type * as stylingExpression from '@/expression.ts'

/**
 * @see https://developer.mozilla.org/en-US/docs/Web/CSS/position
 */
export function position(
  value: stylingExpression.ExpressionOpt<
    'absolute' | 'fixed' | 'relative' | 'static' | 'sticky'
  >,
): stylingAtom.Atom {
  return stylingAtom.atom('position', value)
}
