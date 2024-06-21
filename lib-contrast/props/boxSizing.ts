import * as contrastAtom from '@/atom.ts'
import type * as contrastExpression from '@/expression.ts'

export function boxSizing(
  value: contrastExpression.ExpressionOpt<'border-box' | 'content-box'>,
): contrastAtom.Atom {
  return contrastAtom.atom('box-sizing', value)
}
