import * as contrastAtom from '@/atom.ts'
import type * as contrastData from '@/data/index.ts'
import type * as contrastExpression from '@/expression.ts'

/**
 * @see https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/fill
 */
export function fill(
  value: contrastExpression.ExpressionOpt<contrastData.SvgPaint>,
): contrastAtom.Atom {
  return contrastAtom.atom('fill', value)
}
