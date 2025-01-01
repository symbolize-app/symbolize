import * as stylingAtom from '@/atom.ts'
import type * as stylingData from '@/data/index.ts'
import type * as stylingExpression from '@/expression.ts'

export const padding = {
  /**
   * @see https://developer.mozilla.org/en-US/docs/Web/CSS/padding-top
   */
  t(
    value: stylingExpression.ExpressionOpt<
      stylingData.Length | stylingData.Pct
    >,
  ): stylingAtom.Atom {
    return stylingAtom.atom('padding-top', value)
  },

  /**
   * @see https://developer.mozilla.org/en-US/docs/Web/CSS/padding-right
   */
  r(
    value: stylingExpression.ExpressionOpt<
      stylingData.Length | stylingData.Pct
    >,
  ): stylingAtom.Atom {
    return stylingAtom.atom('padding-right', value)
  },

  /**
   * @see https://developer.mozilla.org/en-US/docs/Web/CSS/padding-bottom
   */
  b(
    value: stylingExpression.ExpressionOpt<
      stylingData.Length | stylingData.Pct
    >,
  ): stylingAtom.Atom {
    return stylingAtom.atom('padding-bottom', value)
  },

  /**
   * @see https://developer.mozilla.org/en-US/docs/Web/CSS/padding-left
   */
  l(
    value: stylingExpression.ExpressionOpt<
      stylingData.Length | stylingData.Pct
    >,
  ): stylingAtom.Atom {
    return stylingAtom.atom('padding-left', value)
  },

  tb(
    value: stylingExpression.ExpressionOpt<
      stylingData.Length | stylingData.Pct
    >,
  ): stylingAtom.AtomOpt {
    return [padding.t(value), padding.b(value)]
  },

  rl(
    value: stylingExpression.ExpressionOpt<
      stylingData.Length | stylingData.Pct
    >,
  ): stylingAtom.AtomOpt {
    return [padding.r(value), padding.l(value)]
  },

  trbl(
    value: stylingExpression.ExpressionOpt<
      stylingData.Length | stylingData.Pct
    >,
  ): stylingAtom.AtomOpt {
    return [
      padding.t(value),
      padding.r(value),
      padding.b(value),
      padding.l(value),
    ]
  },

  /**
   * @see https://developer.mozilla.org/en-US/docs/Web/CSS/padding-block-start
   */
  os(
    value: stylingExpression.ExpressionOpt<
      stylingData.Length | stylingData.Pct
    >,
  ): stylingAtom.Atom {
    return stylingAtom.atom('padding-block-start', value)
  },

  /**
   * @see https://developer.mozilla.org/en-US/docs/Web/CSS/padding-block-end
   */
  oe(
    value: stylingExpression.ExpressionOpt<
      stylingData.Length | stylingData.Pct
    >,
  ): stylingAtom.Atom {
    return stylingAtom.atom('padding-block-end', value)
  },

  /**
   * @see https://developer.mozilla.org/en-US/docs/Web/CSS/padding-inline-start
   */
  is(
    value: stylingExpression.ExpressionOpt<
      stylingData.Length | stylingData.Pct
    >,
  ): stylingAtom.Atom {
    return stylingAtom.atom('padding-inline-start', value)
  },

  /**
   * @see https://developer.mozilla.org/en-US/docs/Web/CSS/padding-inline-end
   */
  ie(
    value: stylingExpression.ExpressionOpt<
      stylingData.Length | stylingData.Pct
    >,
  ): stylingAtom.Atom {
    return stylingAtom.atom('padding-inline-end', value)
  },

  o(
    value: stylingExpression.ExpressionOpt<
      stylingData.Length | stylingData.Pct
    >,
  ): stylingAtom.AtomOpt {
    return [padding.os(value), padding.oe(value)]
  },

  i(
    value: stylingExpression.ExpressionOpt<
      stylingData.Length | stylingData.Pct
    >,
  ): stylingAtom.AtomOpt {
    return [padding.is(value), padding.ie(value)]
  },

  oi(
    value: stylingExpression.ExpressionOpt<
      stylingData.Length | stylingData.Pct
    >,
  ): stylingAtom.AtomOpt {
    return [
      padding.os(value),
      padding.oe(value),
      padding.is(value),
      padding.ie(value),
    ]
  },
}
