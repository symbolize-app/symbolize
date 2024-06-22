import * as contrastAtom from '@/atom.ts'
import type * as contrastData from '@/data/index.ts'
import type * as contrastExpression from '@/expression.ts'

export const inset = {
  /**
   * @see https://developer.mozilla.org/en-US/docs/Web/CSS/top
   */
  t(
    value: contrastExpression.ExpressionOpt<
      contrastData.Length | contrastData.Pct
    >,
  ): contrastAtom.Atom {
    return contrastAtom.atom('top', value)
  },

  /**
   * @see https://developer.mozilla.org/en-US/docs/Web/CSS/right
   */
  r(
    value: contrastExpression.ExpressionOpt<
      contrastData.Length | contrastData.Pct
    >,
  ): contrastAtom.Atom {
    return contrastAtom.atom('right', value)
  },

  /**
   * @see https://developer.mozilla.org/en-US/docs/Web/CSS/bottom
   */
  b(
    value: contrastExpression.ExpressionOpt<
      contrastData.Length | contrastData.Pct
    >,
  ): contrastAtom.Atom {
    return contrastAtom.atom('bottom', value)
  },

  /**
   * @see https://developer.mozilla.org/en-US/docs/Web/CSS/left
   */
  l(
    value: contrastExpression.ExpressionOpt<
      contrastData.Length | contrastData.Pct
    >,
  ): contrastAtom.Atom {
    return contrastAtom.atom('left', value)
  },

  tb(
    value: contrastExpression.ExpressionOpt<
      contrastData.Length | contrastData.Pct
    >,
  ): contrastAtom.AtomOpt {
    return [inset.t(value), inset.b(value)]
  },

  rl(
    value: contrastExpression.ExpressionOpt<
      contrastData.Length | contrastData.Pct
    >,
  ): contrastAtom.AtomOpt {
    return [inset.r(value), inset.l(value)]
  },

  trbl(
    value: contrastExpression.ExpressionOpt<
      contrastData.Length | contrastData.Pct
    >,
  ): contrastAtom.AtomOpt {
    return [inset.t(value), inset.r(value), inset.b(value), inset.l(value)]
  },

  /**
   * @see https://developer.mozilla.org/en-US/docs/Web/CSS/inset-block-start
   */
  os(
    value: contrastExpression.ExpressionOpt<
      contrastData.Length | contrastData.Pct
    >,
  ): contrastAtom.Atom {
    return contrastAtom.atom('inset-block-start', value)
  },

  /**
   * @see https://developer.mozilla.org/en-US/docs/Web/CSS/inset-block-end
   */
  oe(
    value: contrastExpression.ExpressionOpt<
      contrastData.Length | contrastData.Pct
    >,
  ): contrastAtom.Atom {
    return contrastAtom.atom('inset-block-end', value)
  },

  /**
   * @see https://developer.mozilla.org/en-US/docs/Web/CSS/inset-inline-start
   */
  is(
    value: contrastExpression.ExpressionOpt<
      contrastData.Length | contrastData.Pct
    >,
  ): contrastAtom.Atom {
    return contrastAtom.atom('inset-inline-start', value)
  },

  /**
   * @see https://developer.mozilla.org/en-US/docs/Web/CSS/inset-inline-end
   */
  ie(
    value: contrastExpression.ExpressionOpt<
      contrastData.Length | contrastData.Pct
    >,
  ): contrastAtom.Atom {
    return contrastAtom.atom('inset-inline-end', value)
  },

  o(
    value: contrastExpression.ExpressionOpt<
      contrastData.Length | contrastData.Pct
    >,
  ): contrastAtom.AtomOpt {
    return [inset.os(value), inset.oe(value)]
  },

  i(
    value: contrastExpression.ExpressionOpt<
      contrastData.Length | contrastData.Pct
    >,
  ): contrastAtom.AtomOpt {
    return [inset.is(value), inset.ie(value)]
  },

  oi(
    value: contrastExpression.ExpressionOpt<
      contrastData.Length | contrastData.Pct
    >,
  ): contrastAtom.AtomOpt {
    return [
      inset.os(value),
      inset.oe(value),
      inset.is(value),
      inset.ie(value),
    ]
  },
}
