import * as contrastAtom from '@/atom.ts'
import type * as contrastData from '@/data/index.ts'
import type * as contrastExpression from '@/expression.ts'

/**
 * @see https://developer.mozilla.org/en-US/docs/Web/CSS/content
 */
export function content(
  value: contrastExpression.ExpressionOpt<contrastData.String_>,
): contrastAtom.Atom {
  return contrastAtom.atom('content', value)
}
