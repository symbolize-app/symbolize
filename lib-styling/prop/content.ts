import * as stylingAtom from '@/atom.ts'
import type * as stylingData from '@/data/index.ts'
import * as stylingExpression from '@/expression.ts'
import * as stylingExpressionIntern from '@/expressionIntern.ts'

/**
 * @see https://developer.mozilla.org/en-US/docs/Web/CSS/content
 */
export function content(
  value: stylingExpression.ExpressionOpt<stylingData.String_>,
  ...values: readonly stylingExpression.ExpressionOpt<stylingData.String_>[]
): stylingAtom.Atom {
  return stylingAtom.atom(
    'content',
    stylingExpression.expression(
      stylingExpressionIntern.compilePureSpaceSeparator,
      (ctx) => stylingExpression.compileAllToPure(ctx, value, ...values),
    ),
  )
}
