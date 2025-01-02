import * as stylingAtom from '@/atom.ts'
import type * as stylingData from '@/data/index.ts'
import * as stylingExpression from '@/expression.ts'
import * as stylingExpressionIntern from '@/expressionIntern.ts'

export const font = {
  /**
   * @see https://developer.mozilla.org/en-US/docs/Web/CSS/font-size
   */
  size(
    value: stylingExpression.ExpressionOpt<
      | stylingData.Length
      | stylingData.Pct
      | 'large'
      | 'math'
      | 'medium'
      | 'small'
      | 'x-large'
      | 'x-small'
      | 'xx-large'
      | 'xx-small'
      | 'xxx-large'
    >,
  ): stylingAtom.Atom {
    return stylingAtom.atom('font-size', value)
  },

  variant: {
    /**
     * @see https://developer.mozilla.org/en-US/docs/Web/CSS/font-variant-ligatures
     */
    ligatures(
      ...values:
        | Exclude<
            readonly [
              ...(
                | [
                    stylingExpression.ExpressionOpt<
                      'common-ligatures' | 'no-common-ligatures'
                    >,
                  ]
                | []
              ),
              ...(
                | [
                    stylingExpression.ExpressionOpt<
                      | 'discretionary-ligatures'
                      | 'no-discretionary-ligatures'
                    >,
                  ]
                | []
              ),
              ...(
                | [
                    stylingExpression.ExpressionOpt<
                      'historical-ligatures' | 'no-historical-ligatures'
                    >,
                  ]
                | []
              ),
              ...(
                | [
                    stylingExpression.ExpressionOpt<
                      'contextual-ligatures' | 'no-contextual-ligatures'
                    >,
                  ]
                | []
              ),
            ],
            readonly []
          >
        | readonly [stylingExpression.ExpressionOpt<'none' | 'normal'>]
    ): stylingAtom.Atom {
      return stylingAtom.atom(
        'font-variant-ligatures',
        stylingExpression.expression(
          stylingExpressionIntern.compilePureSpaceSeparator,
          (ctx) => stylingExpression.compileAllToPure(ctx, ...values),
        ),
      )
    },

    /**
     * @see https://developer.mozilla.org/en-US/docs/Web/CSS/font-variant-numeric
     */
    numeric(
      ...values:
        | Exclude<
            readonly [
              ...(
                | [
                    stylingExpression.ExpressionOpt<
                      'lining-nums' | 'oldstyle-nums'
                    >,
                  ]
                | []
              ),
              ...(
                | [
                    stylingExpression.ExpressionOpt<
                      'proportional-nums' | 'tabular-nums'
                    >,
                  ]
                | []
              ),
              ...(
                | [
                    stylingExpression.ExpressionOpt<
                      'diagonal-fractions' | 'stacked-fractions'
                    >,
                  ]
                | []
              ),
              ...([] | [stylingExpression.ExpressionOpt<'ordinal'>]),
              ...([] | [stylingExpression.ExpressionOpt<'slashed-zero'>]),
            ],
            readonly []
          >
        | readonly [stylingExpression.ExpressionOpt<'normal'>]
    ): stylingAtom.Atom {
      return stylingAtom.atom(
        'font-variant-numeric',
        stylingExpression.expression(
          stylingExpressionIntern.compilePureSpaceSeparator,
          (ctx) => stylingExpression.compileAllToPure(ctx, ...values),
        ),
      )
    },
  },

  /**
   * @see https://developer.mozilla.org/en-US/docs/Web/CSS/font-weight
   */
  weight(
    value: stylingExpression.ExpressionOpt<
      number | 'bold' | 'bolder' | 'lighter' | 'normal'
    >,
  ): stylingAtom.Atom {
    return stylingAtom.atom('font-weight', value)
  },
}
