import * as contrastAtom from '@/atom.ts'
import type * as contrastData from '@/data/index.ts'
import type * as contrastExpression from '@/expression.ts'

export const margin = {
  /**
   * @see https://developer.mozilla.org/en-US/docs/Web/CSS/margin-top
   */
  t(
    value: contrastExpression.ExpressionOpt<
      contrastData.Length | contrastData.Pct | 'auto'
    >,
  ): contrastAtom.Atom {
    return contrastAtom.atom('margin-top', value)
  },

  /**
   * @see https://developer.mozilla.org/en-US/docs/Web/CSS/margin-right
   */
  r(
    value: contrastExpression.ExpressionOpt<
      contrastData.Length | contrastData.Pct | 'auto'
    >,
  ): contrastAtom.Atom {
    return contrastAtom.atom('margin-right', value)
  },

  /**
   * @see https://developer.mozilla.org/en-US/docs/Web/CSS/margin-bottom
   */
  b(
    value: contrastExpression.ExpressionOpt<
      contrastData.Length | contrastData.Pct | 'auto'
    >,
  ): contrastAtom.Atom {
    return contrastAtom.atom('margin-bottom', value)
  },

  /**
   * @see https://developer.mozilla.org/en-US/docs/Web/CSS/margin-left
   */
  l(
    value: contrastExpression.ExpressionOpt<
      contrastData.Length | contrastData.Pct | 'auto'
    >,
  ): contrastAtom.Atom {
    return contrastAtom.atom('margin-left', value)
  },

  tb(
    value: contrastExpression.ExpressionOpt<
      contrastData.Length | contrastData.Pct | 'auto'
    >,
  ): contrastAtom.AtomOpt {
    return [margin.t(value), margin.b(value)]
  },

  rl(
    value: contrastExpression.ExpressionOpt<
      contrastData.Length | contrastData.Pct | 'auto'
    >,
  ): contrastAtom.AtomOpt {
    return [margin.r(value), margin.l(value)]
  },

  trbl(
    value: contrastExpression.ExpressionOpt<
      contrastData.Length | contrastData.Pct | 'auto'
    >,
  ): contrastAtom.AtomOpt {
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
    value: contrastExpression.ExpressionOpt<
      contrastData.Length | contrastData.Pct | 'auto'
    >,
  ): contrastAtom.Atom {
    return contrastAtom.atom('margin-block-start', value)
  },

  /**
   * @see https://developer.mozilla.org/en-US/docs/Web/CSS/margin-block-end
   */
  oe(
    value: contrastExpression.ExpressionOpt<
      contrastData.Length | contrastData.Pct | 'auto'
    >,
  ): contrastAtom.Atom {
    return contrastAtom.atom('margin-block-end', value)
  },

  /**
   * @see https://developer.mozilla.org/en-US/docs/Web/CSS/margin-inline-start
   */
  is(
    value: contrastExpression.ExpressionOpt<
      contrastData.Length | contrastData.Pct | 'auto'
    >,
  ): contrastAtom.Atom {
    return contrastAtom.atom('margin-inline-start', value)
  },

  /**
   * @see https://developer.mozilla.org/en-US/docs/Web/CSS/margin-inline-end
   */
  ie(
    value: contrastExpression.ExpressionOpt<
      contrastData.Length | contrastData.Pct | 'auto'
    >,
  ): contrastAtom.Atom {
    return contrastAtom.atom('margin-inline-end', value)
  },

  o(
    value: contrastExpression.ExpressionOpt<
      contrastData.Length | contrastData.Pct | 'auto'
    >,
  ): contrastAtom.AtomOpt {
    return [margin.os(value), margin.oe(value)]
  },

  i(
    value: contrastExpression.ExpressionOpt<
      contrastData.Length | contrastData.Pct | 'auto'
    >,
  ): contrastAtom.AtomOpt {
    return [margin.is(value), margin.ie(value)]
  },

  oi(
    value: contrastExpression.ExpressionOpt<
      contrastData.Length | contrastData.Pct | 'auto'
    >,
  ): contrastAtom.AtomOpt {
    return [
      margin.os(value),
      margin.oe(value),
      margin.is(value),
      margin.ie(value),
    ]
  },
}
