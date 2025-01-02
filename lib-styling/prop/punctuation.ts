import * as stylingAtom from '@/atom.ts'
import * as stylingExpression from '@/expression.ts'
import * as stylingExpressionIntern from '@/expressionIntern.ts'

export const punctuation = {
  /**
   * @see https://developer.mozilla.org/en-US/docs/Web/CSS/hanging-punctuation
   */
  hang(
    ...values:
      | Exclude<
          readonly [
            ...([] | [stylingExpression.ExpressionOpt<'first'>]),
            ...([] | [stylingExpression.ExpressionOpt<'allow-end'>]),
            ...([] | [stylingExpression.ExpressionOpt<'last'>]),
          ],
          readonly []
        >
      | readonly [stylingExpression.ExpressionOpt<'none'>]
  ): stylingAtom.Atom {
    return stylingAtom.atom(
      'hanging-punctuation',
      stylingExpression.expression(
        stylingExpressionIntern.compilePureSpaceSeparator,
        (ctx) => stylingExpression.compileAllToPure(ctx, ...values),
      ),
    )
  },
}
