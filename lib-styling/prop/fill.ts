import * as stylingAtom from '@/atom.ts'
import type * as stylingData from '@/data/index.ts'
import type * as stylingExpression from '@/expression.ts'

/**
 * @see https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/fill
 */
export function fill(
  value: stylingExpression.ExpressionOpt<stylingData.SvgPaint>,
): stylingAtom.Atom {
  return stylingAtom.atom('fill', value)
}
