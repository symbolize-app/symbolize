import * as contrastAtom from '@/atom.ts'
import type * as contrastData from '@/data/index.ts'
import type * as contrastExpression from '@/expression.ts'

export function fill(
  value: contrastExpression.ExpressionOpt<contrastData.SvgPaint>,
): contrastAtom.Atom {
  return contrastAtom.atom('fill', value)
}
