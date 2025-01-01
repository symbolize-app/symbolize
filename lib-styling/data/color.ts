import type * as stylingDataAngle from '@/data/angle.ts'
import type * as stylingDataPercentage from '@/data/percentage.ts'
import * as stylingExpression from '@/expression.ts'
import * as stylingExpressionIntern from '@/expressionIntern.ts'

const colorMarker = Symbol('colorMarker')

/**
 * @see https://developer.mozilla.org/en-US/docs/Web/CSS/color_value
 */
export interface Color {
  [colorMarker](): unknown
}

/**
 * @see https://developer.mozilla.org/en-US/docs/Web/CSS/color_value/light-dark
 */
export function lightDark(
  light: stylingExpression.ExpressionOpt<Color>,
  dark: stylingExpression.ExpressionOpt<Color>,
): stylingExpression.Expression<Color> {
  return stylingExpression.expression(
    stylingExpressionIntern.compilePureFunction,
    (ctx) => [
      'light-dark',
      ...stylingExpression.compileAllToPure(ctx, light, dark),
    ],
  )
}

/**
 * @see https://developer.mozilla.org/en-US/docs/Web/CSS/color_value/rgb
 */
export function hsl(
  ...values: readonly [
    h: stylingExpression.ExpressionOpt<stylingDataAngle.Angle>,
    s: stylingExpression.ExpressionOpt<stylingDataPercentage.Pct>,
    l: stylingExpression.ExpressionOpt<stylingDataPercentage.Pct>,
    ...(
      | []
      | [a: stylingExpression.ExpressionOpt<stylingDataPercentage.Pct>]
    ),
  ]
): stylingExpression.Expression<Color> {
  return stylingExpression.expression(compileColor, (ctx) => [
    'hsl',
    ...stylingExpression.compileAllToPure(ctx, ...values),
  ])
}

/**
 * @see https://developer.mozilla.org/en-US/docs/Web/CSS/color_value/rgb
 */
export function rgb(
  ...values: readonly [
    r: stylingExpression.ExpressionOpt<stylingDataPercentage.Pct>,
    g: stylingExpression.ExpressionOpt<stylingDataPercentage.Pct>,
    b: stylingExpression.ExpressionOpt<stylingDataPercentage.Pct>,
    ...(
      | []
      | [a: stylingExpression.ExpressionOpt<stylingDataPercentage.Pct>]
    ),
  ]
): stylingExpression.Expression<Color> {
  return stylingExpression.expression(compileColor, (ctx) => [
    'rgb',
    ...stylingExpression.compileAllToPure(ctx, ...values),
  ])
}

export function compileColor(
  name: string,
  ...values: readonly [
    stylingExpressionIntern.PureExpressionIntern,
    stylingExpressionIntern.PureExpressionIntern,
    stylingExpressionIntern.PureExpressionIntern,
    ...([] | [stylingExpressionIntern.PureExpressionIntern]),
  ]
): stylingExpressionIntern.PureExpressionIntern {
  return stylingExpressionIntern.compilePure(
    values.length === 3 ?
      `${name}(${values[0].value} ${values[1].value} ${values[2].value})`
    : `${name}(${values[0].value} ${values[1].value} ${values[2].value} / ${values[3].value})`,
    ...values,
  )
}
