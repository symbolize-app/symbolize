import * as conveyAtom from '@/atom.ts'
import type * as conveyData from '@/data.ts'
import type * as conveyExpression from '@/expression.ts'

export const background = {
  color(
    value: conveyExpression.ExpressionOpt<
      conveyData.Color,
      conveyExpression.FullScope
    >,
  ): conveyAtom.Atom {
    return new conveyAtom.Atom('background-color', value)
  },
}
