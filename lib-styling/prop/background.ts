import * as stylingAtom from '@/atom.ts'
import type * as stylingData from '@/data/index.ts'
import * as stylingExpression from '@/expression.ts'
import * as stylingExpressionIntern from '@/expressionIntern.ts'

type Size =
  | stylingExpression.ExpressionOpt<'contain' | 'cover'>
  | readonly [
      stylingExpression.ExpressionOpt<
        stylingData.Length | stylingData.Pct | 'auto'
      >,
      stylingExpression.ExpressionOpt<
        stylingData.Length | stylingData.Pct | 'auto'
      >,
    ]

export const background = {
  /**
   * @see https://developer.mozilla.org/en-US/docs/Web/CSS/background-color
   */
  color(
    value: stylingExpression.ExpressionOpt<stylingData.Color>,
  ): stylingAtom.Atom {
    return stylingAtom.atom('background-color', value)
  },

  /**
   * @see https://developer.mozilla.org/en-US/docs/Web/CSS/background-image
   */
  image(
    value: stylingExpression.ExpressionOpt<stylingData.Image>,
    ...values: readonly stylingExpression.ExpressionOpt<stylingData.Image>[]
  ): stylingAtom.Atom {
    return stylingAtom.atom(
      'background-image',
      stylingExpression.expression(
        stylingExpressionIntern.compilePureCommaSeparator,
        (ctx) => stylingExpression.compileAllToPure(ctx, value, ...values),
      ),
    )
  },

  /**
   * @see https://developer.mozilla.org/en-US/docs/Web/CSS/background-size
   */
  size(value: Size, ...values: readonly Size[]): stylingAtom.Atom {
    return stylingAtom.atom(
      'background-size',
      stylingExpression.expression(
        stylingExpressionIntern.compilePureCommaSeparator,
        (ctx) =>
          stylingExpression.compileAllToPure(
            ctx,
            ...[value, ...values].map((value) =>
              Array.isArray(value) ?
                stylingExpression.expression(
                  stylingExpressionIntern.compilePureSpaceSeparator,
                  (ctx) =>
                    stylingExpression.compileAllToPure(ctx, ...value),
                )
              : value,
            ),
          ),
      ),
    )
  },
}
