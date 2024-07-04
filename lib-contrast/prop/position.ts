import * as contrastAtom from '@/atom.ts'
import type * as contrastExpression from '@/expression.ts'

/**
 * @see https://developer.mozilla.org/en-US/docs/Web/CSS/position
 */
export function position(
  value: contrastExpression.ExpressionOpt<
    'absolute' | 'fixed' | 'relative' | 'static' | 'sticky'
  >,
): contrastAtom.Atom {
  return contrastAtom.atom('position', value)
}
