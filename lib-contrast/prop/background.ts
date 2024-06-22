import * as contrastAtom from '@/atom.ts'
import type * as contrastData from '@/data/index.ts'
import * as contrastExpression from '@/expression.ts'
import * as contrastExpressionIntern from '@/expressionIntern.ts'

type Size =
  | contrastExpression.ExpressionOpt<'contain' | 'cover'>
  | readonly [
      contrastExpression.ExpressionOpt<
        contrastData.Length | contrastData.Pct | 'auto'
      >,
      contrastExpression.ExpressionOpt<
        contrastData.Length | contrastData.Pct | 'auto'
      >,
    ]

export const background = {
  /**
   * @see https://developer.mozilla.org/en-US/docs/Web/CSS/background-color
   */
  color(
    value: contrastExpression.ExpressionOpt<contrastData.Color>,
  ): contrastAtom.Atom {
    return contrastAtom.atom('background-color', value)
  },

  /**
   * @see https://developer.mozilla.org/en-US/docs/Web/CSS/background-image
   */
  image(
    value: contrastExpression.ExpressionOpt<contrastData.Image>,
    ...values: readonly contrastExpression.ExpressionOpt<contrastData.Image>[]
  ): contrastAtom.Atom {
    return contrastAtom.atom(
      'background-image',
      contrastExpression.expression(
        contrastExpressionIntern.compilePureCommaSeparator,
        (ctx) =>
          contrastExpression.compileAllToPure(ctx, value, ...values),
      ),
    )
  },

  /**
   * @see https://developer.mozilla.org/en-US/docs/Web/CSS/background-size
   */
  size(value: Size, ...values: readonly Size[]): contrastAtom.Atom {
    return contrastAtom.atom(
      'background-size',
      contrastExpression.expression(
        contrastExpressionIntern.compilePureCommaSeparator,
        (ctx) =>
          contrastExpression.compileAllToPure(
            ctx,
            ...[value, ...values].map((value) =>
              Array.isArray(value) ?
                contrastExpression.expression(
                  contrastExpressionIntern.compilePureSpaceSeparator,
                  (ctx) =>
                    contrastExpression.compileAllToPure(ctx, ...value),
                )
              : value,
            ),
          ),
      ),
    )
  },
}
