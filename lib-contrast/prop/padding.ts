import * as contrastAtom from '@/atom.ts'
import type * as contrastData from '@/data/index.ts'
import type * as contrastExpression from '@/expression.ts'

export const padding = {
  /**
   * @see https://developer.mozilla.org/en-US/docs/Web/CSS/padding-top
   */
  t(
    value: contrastExpression.ExpressionOpt<
      contrastData.Length | contrastData.Pct
    >,
  ): contrastAtom.Atom {
    return contrastAtom.atom('padding-top', value)
  },

  /**
   * @see https://developer.mozilla.org/en-US/docs/Web/CSS/padding-right
   */
  r(
    value: contrastExpression.ExpressionOpt<
      contrastData.Length | contrastData.Pct
    >,
  ): contrastAtom.Atom {
    return contrastAtom.atom('padding-right', value)
  },

  /**
   * @see https://developer.mozilla.org/en-US/docs/Web/CSS/padding-bottom
   */
  b(
    value: contrastExpression.ExpressionOpt<
      contrastData.Length | contrastData.Pct
    >,
  ): contrastAtom.Atom {
    return contrastAtom.atom('padding-bottom', value)
  },

  /**
   * @see https://developer.mozilla.org/en-US/docs/Web/CSS/padding-left
   */
  l(
    value: contrastExpression.ExpressionOpt<
      contrastData.Length | contrastData.Pct
    >,
  ): contrastAtom.Atom {
    return contrastAtom.atom('padding-left', value)
  },

  tb(
    value: contrastExpression.ExpressionOpt<
      contrastData.Length | contrastData.Pct
    >,
  ): contrastAtom.AtomOpt {
    return [padding.t(value), padding.b(value)]
  },

  rl(
    value: contrastExpression.ExpressionOpt<
      contrastData.Length | contrastData.Pct
    >,
  ): contrastAtom.AtomOpt {
    return [padding.r(value), padding.l(value)]
  },

  trbl(
    value: contrastExpression.ExpressionOpt<
      contrastData.Length | contrastData.Pct
    >,
  ): contrastAtom.AtomOpt {
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
    value: contrastExpression.ExpressionOpt<
      contrastData.Length | contrastData.Pct
    >,
  ): contrastAtom.Atom {
    return contrastAtom.atom('padding-block-start', value)
  },

  /**
   * @see https://developer.mozilla.org/en-US/docs/Web/CSS/padding-block-end
   */
  oe(
    value: contrastExpression.ExpressionOpt<
      contrastData.Length | contrastData.Pct
    >,
  ): contrastAtom.Atom {
    return contrastAtom.atom('padding-block-end', value)
  },

  /**
   * @see https://developer.mozilla.org/en-US/docs/Web/CSS/padding-inline-start
   */
  is(
    value: contrastExpression.ExpressionOpt<
      contrastData.Length | contrastData.Pct
    >,
  ): contrastAtom.Atom {
    return contrastAtom.atom('padding-inline-start', value)
  },

  /**
   * @see https://developer.mozilla.org/en-US/docs/Web/CSS/padding-inline-end
   */
  ie(
    value: contrastExpression.ExpressionOpt<
      contrastData.Length | contrastData.Pct
    >,
  ): contrastAtom.Atom {
    return contrastAtom.atom('padding-inline-end', value)
  },

  o(
    value: contrastExpression.ExpressionOpt<
      contrastData.Length | contrastData.Pct
    >,
  ): contrastAtom.AtomOpt {
    return [padding.os(value), padding.oe(value)]
  },

  i(
    value: contrastExpression.ExpressionOpt<
      contrastData.Length | contrastData.Pct
    >,
  ): contrastAtom.AtomOpt {
    return [padding.is(value), padding.ie(value)]
  },

  oi(
    value: contrastExpression.ExpressionOpt<
      contrastData.Length | contrastData.Pct
    >,
  ): contrastAtom.AtomOpt {
    return [
      padding.os(value),
      padding.oe(value),
      padding.is(value),
      padding.ie(value),
    ]
  },
}
