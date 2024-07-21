import * as contrastAtom from '@/atom.ts'
import type * as contrastData from '@/data/index.ts'
import * as contrastExpression from '@/expression.ts'
import * as contrastExpressionIntern from '@/expressionIntern.ts'

/**
 * @see https://developer.mozilla.org/en-US/docs/Web/CSS/content
 */
export function content(
  value: contrastExpression.ExpressionOpt<contrastData.String_>,
  ...values: readonly contrastExpression.ExpressionOpt<contrastData.String_>[]
): contrastAtom.Atom {
  return contrastAtom.atom(
    'content',
    contrastExpression.expression(
      contrastExpressionIntern.compilePureSpaceSeparator,
      (ctx) => contrastExpression.compileAllToPure(ctx, value, ...values),
    ),
  )
}
