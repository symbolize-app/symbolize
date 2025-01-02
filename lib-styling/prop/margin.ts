import * as stylingAtom from '@/atom.ts'
import type * as stylingData from '@/data/index.ts'
import type * as stylingExpression from '@/expression.ts'

export const margin = {
  /**
   * @see https://developer.mozilla.org/en-US/docs/Web/CSS/margin-top
   */
  t(
    value: stylingExpression.ExpressionOpt<
      stylingData.Length | stylingData.Pct | 'auto'
    >,
  ): stylingAtom.Atom {
    return stylingAtom.atom('margin-top', value)
  },

  /**
   * @see https://developer.mozilla.org/en-US/docs/Web/CSS/margin-right
   */
  r(
    value: stylingExpression.ExpressionOpt<
      stylingData.Length | stylingData.Pct | 'auto'
    >,
  ): stylingAtom.Atom {
    return stylingAtom.atom('margin-right', value)
  },

  /**
   * @see https://developer.mozilla.org/en-US/docs/Web/CSS/margin-bottom
   */
  b(
    value: stylingExpression.ExpressionOpt<
      stylingData.Length | stylingData.Pct | 'auto'
    >,
  ): stylingAtom.Atom {
    return stylingAtom.atom('margin-bottom', value)
  },

  /**
   * @see https://developer.mozilla.org/en-US/docs/Web/CSS/margin-left
   */
  l(
    value: stylingExpression.ExpressionOpt<
      stylingData.Length | stylingData.Pct | 'auto'
    >,
  ): stylingAtom.Atom {
    return stylingAtom.atom('margin-left', value)
  },

  tb(
    value: stylingExpression.ExpressionOpt<
      stylingData.Length | stylingData.Pct | 'auto'
    >,
  ): stylingAtom.AtomOpt {
    return [margin.t(value), margin.b(value)]
  },

  rl(
    value: stylingExpression.ExpressionOpt<
      stylingData.Length | stylingData.Pct | 'auto'
    >,
  ): stylingAtom.AtomOpt {
    return [margin.r(value), margin.l(value)]
  },

  trbl(
    value: stylingExpression.ExpressionOpt<
      stylingData.Length | stylingData.Pct | 'auto'
    >,
  ): stylingAtom.AtomOpt {
    return [
      margin.t(value),
      margin.r(value),
      margin.b(value),
      margin.l(value),
    ]
  },

  /**
   * @see https://developer.mozilla.org/en-US/docs/Web/CSS/margin-block-start
   */
  os(
    value: stylingExpression.ExpressionOpt<
      stylingData.Length | stylingData.Pct | 'auto'
    >,
  ): stylingAtom.Atom {
    return stylingAtom.atom('margin-block-start', value)
  },

  /**
   * @see https://developer.mozilla.org/en-US/docs/Web/CSS/margin-block-end
   */
  oe(
    value: stylingExpression.ExpressionOpt<
      stylingData.Length | stylingData.Pct | 'auto'
    >,
  ): stylingAtom.Atom {
    return stylingAtom.atom('margin-block-end', value)
  },

  /**
   * @see https://developer.mozilla.org/en-US/docs/Web/CSS/margin-inline-start
   */
  is(
    value: stylingExpression.ExpressionOpt<
      stylingData.Length | stylingData.Pct | 'auto'
    >,
  ): stylingAtom.Atom {
    return stylingAtom.atom('margin-inline-start', value)
  },

  /**
   * @see https://developer.mozilla.org/en-US/docs/Web/CSS/margin-inline-end
   */
  ie(
    value: stylingExpression.ExpressionOpt<
      stylingData.Length | stylingData.Pct | 'auto'
    >,
  ): stylingAtom.Atom {
    return stylingAtom.atom('margin-inline-end', value)
  },

  o(
    value: stylingExpression.ExpressionOpt<
      stylingData.Length | stylingData.Pct | 'auto'
    >,
  ): stylingAtom.AtomOpt {
    return [margin.os(value), margin.oe(value)]
  },

  i(
    value: stylingExpression.ExpressionOpt<
      stylingData.Length | stylingData.Pct | 'auto'
    >,
  ): stylingAtom.AtomOpt {
    return [margin.is(value), margin.ie(value)]
  },

  oi(
    value: stylingExpression.ExpressionOpt<
      stylingData.Length | stylingData.Pct | 'auto'
    >,
  ): stylingAtom.AtomOpt {
    return [
      margin.os(value),
      margin.oe(value),
      margin.is(value),
      margin.ie(value),
    ]
  },
}
