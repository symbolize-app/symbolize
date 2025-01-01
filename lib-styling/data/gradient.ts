import type * as stylingDataAngle from '@/data/angle.ts'
import type * as stylingDataColor from '@/data/color.ts'
import type * as stylingDataLength from '@/data/length.ts'
import type * as stylingDataPercentage from '@/data/percentage.ts'
import * as stylingExpression from '@/expression.ts'
import * as stylingExpressionIntern from '@/expressionIntern.ts'

const gradientMarker = Symbol('gradientMarker')

/**
 * @see https://developer.mozilla.org/en-US/docs/Web/CSS/gradient
 */
export interface Gradient {
  [gradientMarker](): unknown
}

type GradientStop =
  | stylingExpression.ExpressionOpt<stylingDataColor.Color>
  | readonly [
      stylingExpression.ExpressionOpt<stylingDataColor.Color>,
      stylingExpression.ExpressionOpt<
        stylingDataLength.Length | stylingDataPercentage.Pct
      >,
      stylingExpression.ExpressionOpt<
        stylingDataLength.Length | stylingDataPercentage.Pct
      >,
    ]
  | readonly [
      stylingExpression.ExpressionOpt<stylingDataColor.Color>,
      stylingExpression.ExpressionOpt<
        stylingDataLength.Length | stylingDataPercentage.Pct
      >,
    ]

type GradientHint = stylingExpression.ExpressionOpt<
  stylingDataLength.Length | stylingDataPercentage.Pct
>

type GradientHintStopOpt = readonly [
  ...([] | [GradientHint]),
  GradientStop,
]

type GradientHintStopTuple =
  | readonly [
      ...GradientHintStopOpt,
      ...(
        | [
            ...GradientHintStopOpt,
            ...(
              | [
                  ...GradientHintStopOpt,
                  ...(
                    | [
                        ...GradientHintStopOpt,
                        ...(
                          | [...GradientHintStopOpt, ...GradientHint[]]
                          | []
                        ),
                      ]
                    | []
                  ),
                ]
              | []
            ),
          ]
        | []
      ),
    ]
  | readonly []

export const gradient = {
  /**
   * @see https://developer.mozilla.org/en-US/docs/Web/CSS/gradient/linear-gradient
   */
  linear(
    angle: stylingExpression.ExpressionOpt<stylingDataAngle.Angle>,
    stop: GradientStop,
    ...stops: GradientHintStopTuple
  ): stylingExpression.Expression<Gradient> {
    return stylingExpression.expression(
      stylingExpressionIntern.compilePureFunction,
      (ctx) => [
        'linear-gradient',
        ...stylingExpression.compileAllToPure(
          ctx,
          angle,
          ...[stop, ...stops].map((stop) =>
            Array.isArray(stop) ?
              stylingExpression.expression(
                stylingExpressionIntern.compilePureSpaceSeparator,
                (ctx) => stylingExpression.compileAllToPure(ctx, ...stop),
              )
            : stop,
          ),
        ),
      ],
    )
  },
}
