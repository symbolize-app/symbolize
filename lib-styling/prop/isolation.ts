import * as stylingAtom from '@/atom.ts'
import type * as stylingExpression from '@/expression.ts'

/**
 * @see https://developer.mozilla.org/en-US/docs/Web/CSS/isolation
 */
export function isolation(
  value: stylingExpression.ExpressionOpt<'auto' | 'isolate'>,
): stylingAtom.Atom {
  return stylingAtom.atom('isolation', value)
}
