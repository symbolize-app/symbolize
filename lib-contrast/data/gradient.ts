import type * as contrastDataAngle from '@/data/angle.ts'
import type * as contrastDataColor from '@/data/color.ts'
import type * as contrastDataLength from '@/data/length.ts'
import type * as contrastDataPercentage from '@/data/percentage.ts'
import * as contrastExpression from '@/expression.ts'
import * as contrastExpressionIntern from '@/expressionIntern.ts'

const gradientMarker = Symbol('gradientMarker')

/**
 * @see https://developer.mozilla.org/en-US/docs/Web/CSS/gradient
 */
export interface Gradient {
  [gradientMarker](): unknown
}

type GradientStop =
  | contrastExpression.ExpressionOpt<contrastDataColor.Color>
  | readonly [
      contrastExpression.ExpressionOpt<contrastDataColor.Color>,
      contrastExpression.ExpressionOpt<
        contrastDataLength.Length | contrastDataPercentage.Pct
      >,
      contrastExpression.ExpressionOpt<
        contrastDataLength.Length | contrastDataPercentage.Pct
      >,
    ]
  | readonly [
      contrastExpression.ExpressionOpt<contrastDataColor.Color>,
      contrastExpression.ExpressionOpt<
        contrastDataLength.Length | contrastDataPercentage.Pct
      >,
    ]

type GradientHint = contrastExpression.ExpressionOpt<
  contrastDataLength.Length | contrastDataPercentage.Pct
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
    angle: contrastExpression.ExpressionOpt<contrastDataAngle.Angle>,
    stop: GradientStop,
    ...stops: GradientHintStopTuple
  ): contrastExpression.Expression<Gradient> {
    return contrastExpression.expression(
      contrastExpressionIntern.compilePureFunction,
      (ctx) => [
        'linear-gradient',
        ...contrastExpression.compileAllToPure(
          ctx,
          angle,
          ...[stop, ...stops].map((stop) =>
            Array.isArray(stop) ?
              contrastExpression.expression(
                contrastExpressionIntern.compilePureSpaceSeparator,
                (ctx) => contrastExpression.compileAllToPure(ctx, ...stop),
              )
            : stop,
          ),
        ),
      ],
    )
  },
}
