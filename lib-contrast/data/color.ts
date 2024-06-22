import type * as contrastDataAngle from '@/data/angle.ts'
import type * as contrastDataPercentage from '@/data/percentage.ts'
import * as contrastExpression from '@/expression.ts'
import * as contrastExpressionIntern from '@/expressionIntern.ts'

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
  light: contrastExpression.ExpressionOpt<Color>,
  dark: contrastExpression.ExpressionOpt<Color>,
): contrastExpression.Expression<Color> {
  return contrastExpression.expression(
    contrastExpressionIntern.compilePureFunction,
    (ctx) => [
      'light-dark',
      ...contrastExpression.compileAllToPure(ctx, light, dark),
    ],
  )
}

/**
 * @see https://developer.mozilla.org/en-US/docs/Web/CSS/color_value/rgb
 */
export function hsl(
  ...values: readonly [
    h: contrastExpression.ExpressionOpt<contrastDataAngle.Angle>,
    s: contrastExpression.ExpressionOpt<contrastDataPercentage.Pct>,
    l: contrastExpression.ExpressionOpt<contrastDataPercentage.Pct>,
    ...(
      | []
      | [a: contrastExpression.ExpressionOpt<contrastDataPercentage.Pct>]
    ),
  ]
): contrastExpression.Expression<Color> {
  return contrastExpression.expression(compileColor, (ctx) => [
    'hsl',
    ...contrastExpression.compileAllToPure(ctx, ...values),
  ])
}

/**
 * @see https://developer.mozilla.org/en-US/docs/Web/CSS/color_value/rgb
 */
export function rgb(
  ...values: readonly [
    r: contrastExpression.ExpressionOpt<contrastDataPercentage.Pct>,
    g: contrastExpression.ExpressionOpt<contrastDataPercentage.Pct>,
    b: contrastExpression.ExpressionOpt<contrastDataPercentage.Pct>,
    ...(
      | []
      | [a: contrastExpression.ExpressionOpt<contrastDataPercentage.Pct>]
    ),
  ]
): contrastExpression.Expression<Color> {
  return contrastExpression.expression(compileColor, (ctx) => [
    'rgb',
    ...contrastExpression.compileAllToPure(ctx, ...values),
  ])
}

export function compileColor(
  name: string,
  ...values: readonly [
    contrastExpressionIntern.PureExpressionIntern,
    contrastExpressionIntern.PureExpressionIntern,
    contrastExpressionIntern.PureExpressionIntern,
    ...([] | [contrastExpressionIntern.PureExpressionIntern]),
  ]
): contrastExpressionIntern.PureExpressionIntern {
  return contrastExpressionIntern.compilePure(
    values.length === 3 ?
      `${name}(${values[0].value} ${values[1].value} ${values[2].value})`
    : `${name}(${values[0].value} ${values[1].value} ${values[2].value} / ${values[3].value})`,
    ...values,
  )
}
