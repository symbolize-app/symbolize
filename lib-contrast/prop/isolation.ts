import * as contrastAtom from '@/atom.ts'
import type * as contrastExpression from '@/expression.ts'

/**
 * @see https://developer.mozilla.org/en-US/docs/Web/CSS/isolation
 */
export function isolation(
  value: contrastExpression.ExpressionOpt<'auto' | 'isolate'>,
): contrastAtom.Atom {
  return contrastAtom.atom('isolation', value)
}
