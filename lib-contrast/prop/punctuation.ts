import * as contrastAtom from '@/atom.ts'
import * as contrastExpression from '@/expression.ts'
import * as contrastExpressionIntern from '@/expressionIntern.ts'

export const punctuation = {
  /**
   * @see https://developer.mozilla.org/en-US/docs/Web/CSS/hanging-punctuation
   */
  hang(
    ...values:
      | Exclude<
          readonly [
            ...([] | [contrastExpression.ExpressionOpt<'first'>]),
            ...([] | [contrastExpression.ExpressionOpt<'allow-end'>]),
            ...([] | [contrastExpression.ExpressionOpt<'last'>]),
          ],
          readonly []
        >
      | readonly [contrastExpression.ExpressionOpt<'none'>]
  ): contrastAtom.Atom {
    return contrastAtom.atom(
      'hanging-punctuation',
      contrastExpression.expression(
        contrastExpressionIntern.compilePureSpaceSeparator,
        (ctx) => contrastExpression.compileAllToPure(ctx, ...values),
      ),
    )
  },
}
