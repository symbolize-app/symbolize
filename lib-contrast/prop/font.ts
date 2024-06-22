import * as contrastAtom from '@/atom.ts'
import type * as contrastData from '@/data/index.ts'
import * as contrastExpression from '@/expression.ts'
import * as contrastExpressionIntern from '@/expressionIntern.ts'

export const font = {
  /**
   * @see https://developer.mozilla.org/en-US/docs/Web/CSS/font-size
   */
  size(
    value: contrastExpression.ExpressionOpt<
      | contrastData.Length
      | contrastData.Pct
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
  ): contrastAtom.Atom {
    return contrastAtom.atom('font-size', value)
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
                    contrastExpression.ExpressionOpt<
                      'common-ligatures' | 'no-common-ligatures'
                    >,
                  ]
                | []
              ),
              ...(
                | [
                    contrastExpression.ExpressionOpt<
                      | 'discretionary-ligatures'
                      | 'no-discretionary-ligatures'
                    >,
                  ]
                | []
              ),
              ...(
                | [
                    contrastExpression.ExpressionOpt<
                      'historical-ligatures' | 'no-historical-ligatures'
                    >,
                  ]
                | []
              ),
              ...(
                | [
                    contrastExpression.ExpressionOpt<
                      'contextual-ligatures' | 'no-contextual-ligatures'
                    >,
                  ]
                | []
              ),
            ],
            readonly []
          >
        | readonly [contrastExpression.ExpressionOpt<'none' | 'normal'>]
    ): contrastAtom.Atom {
      return contrastAtom.atom(
        'font-variant-ligatures',
        contrastExpression.expression(
          contrastExpressionIntern.compilePureSpaceSeparator,
          (ctx) => contrastExpression.compileAllToPure(ctx, ...values),
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
                    contrastExpression.ExpressionOpt<
                      'lining-nums' | 'oldstyle-nums'
                    >,
                  ]
                | []
              ),
              ...(
                | [
                    contrastExpression.ExpressionOpt<
                      'proportional-nums' | 'tabular-nums'
                    >,
                  ]
                | []
              ),
              ...(
                | [
                    contrastExpression.ExpressionOpt<
                      'diagonal-fractions' | 'stacked-fractions'
                    >,
                  ]
                | []
              ),
              ...([] | [contrastExpression.ExpressionOpt<'ordinal'>]),
              ...([] | [contrastExpression.ExpressionOpt<'slashed-zero'>]),
            ],
            readonly []
          >
        | readonly [contrastExpression.ExpressionOpt<'normal'>]
    ): contrastAtom.Atom {
      return contrastAtom.atom(
        'font-variant-numeric',
        contrastExpression.expression(
          contrastExpressionIntern.compilePureSpaceSeparator,
          (ctx) => contrastExpression.compileAllToPure(ctx, ...values),
        ),
      )
    },
  },

  /**
   * @see https://developer.mozilla.org/en-US/docs/Web/CSS/font-weight
   */
  weight(
    value: contrastExpression.ExpressionOpt<
      number | 'bold' | 'bolder' | 'lighter' | 'normal'
    >,
  ): contrastAtom.Atom {
    return contrastAtom.atom('font-weight', value)
  },
}
