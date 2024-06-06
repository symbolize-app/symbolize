import * as contrastAtom from '@/atom.ts'
import type * as contrastData from '@/data/index.ts'
import type * as contrastExpression from '@/expression.ts'

export const background = {
  color(
    value: contrastExpression.ExpressionOpt<contrastData.Color>,
  ): contrastAtom.Atom {
    return contrastAtom.atom('background-color', value)
  },
}

export function boxSizing(
  value: contrastExpression.ExpressionOpt<'border-box' | 'content-box'>,
): contrastAtom.Atom {
  return contrastAtom.atom('box-sizing', value)
}

export function fill(
  value: contrastExpression.ExpressionOpt<contrastData.SvgPaint>,
): contrastAtom.Atom {
  return contrastAtom.atom('fill', value)
}
