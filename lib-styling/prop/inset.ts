import * as stylingAtom from '@/atom.ts'
import type * as stylingData from '@/data/index.ts'
import type * as stylingExpression from '@/expression.ts'

export const inset = {
  /**
   * @see https://developer.mozilla.org/en-US/docs/Web/CSS/top
   */
  t(
    value: stylingExpression.ExpressionOpt<
      stylingData.Length | stylingData.Pct
    >,
  ): stylingAtom.Atom {
    return stylingAtom.atom('top', value)
  },

  /**
   * @see https://developer.mozilla.org/en-US/docs/Web/CSS/right
   */
  r(
    value: stylingExpression.ExpressionOpt<
      stylingData.Length | stylingData.Pct
    >,
  ): stylingAtom.Atom {
    return stylingAtom.atom('right', value)
  },

  /**
   * @see https://developer.mozilla.org/en-US/docs/Web/CSS/bottom
   */
  b(
    value: stylingExpression.ExpressionOpt<
      stylingData.Length | stylingData.Pct
    >,
  ): stylingAtom.Atom {
    return stylingAtom.atom('bottom', value)
  },

  /**
   * @see https://developer.mozilla.org/en-US/docs/Web/CSS/left
   */
  l(
    value: stylingExpression.ExpressionOpt<
      stylingData.Length | stylingData.Pct
    >,
  ): stylingAtom.Atom {
    return stylingAtom.atom('left', value)
  },

  tb(
    value: stylingExpression.ExpressionOpt<
      stylingData.Length | stylingData.Pct
    >,
  ): stylingAtom.AtomOpt {
    return [inset.t(value), inset.b(value)]
  },

  rl(
    value: stylingExpression.ExpressionOpt<
      stylingData.Length | stylingData.Pct
    >,
  ): stylingAtom.AtomOpt {
    return [inset.r(value), inset.l(value)]
  },

  trbl(
    value: stylingExpression.ExpressionOpt<
      stylingData.Length | stylingData.Pct
    >,
  ): stylingAtom.AtomOpt {
    return [inset.t(value), inset.r(value), inset.b(value), inset.l(value)]
  },

  /**
   * @see https://developer.mozilla.org/en-US/docs/Web/CSS/inset-block-start
   */
  os(
    value: stylingExpression.ExpressionOpt<
      stylingData.Length | stylingData.Pct
    >,
  ): stylingAtom.Atom {
    return stylingAtom.atom('inset-block-start', value)
  },

  /**
   * @see https://developer.mozilla.org/en-US/docs/Web/CSS/inset-block-end
   */
  oe(
    value: stylingExpression.ExpressionOpt<
      stylingData.Length | stylingData.Pct
    >,
  ): stylingAtom.Atom {
    return stylingAtom.atom('inset-block-end', value)
  },

  /**
   * @see https://developer.mozilla.org/en-US/docs/Web/CSS/inset-inline-start
   */
  is(
    value: stylingExpression.ExpressionOpt<
      stylingData.Length | stylingData.Pct
    >,
  ): stylingAtom.Atom {
    return stylingAtom.atom('inset-inline-start', value)
  },

  /**
   * @see https://developer.mozilla.org/en-US/docs/Web/CSS/inset-inline-end
   */
  ie(
    value: stylingExpression.ExpressionOpt<
      stylingData.Length | stylingData.Pct
    >,
  ): stylingAtom.Atom {
    return stylingAtom.atom('inset-inline-end', value)
  },

  o(
    value: stylingExpression.ExpressionOpt<
      stylingData.Length | stylingData.Pct
    >,
  ): stylingAtom.AtomOpt {
    return [inset.os(value), inset.oe(value)]
  },

  i(
    value: stylingExpression.ExpressionOpt<
      stylingData.Length | stylingData.Pct
    >,
  ): stylingAtom.AtomOpt {
    return [inset.is(value), inset.ie(value)]
  },

  oi(
    value: stylingExpression.ExpressionOpt<
      stylingData.Length | stylingData.Pct
    >,
  ): stylingAtom.AtomOpt {
    return [
      inset.os(value),
      inset.oe(value),
      inset.is(value),
      inset.ie(value),
    ]
  },
}
