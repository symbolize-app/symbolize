import * as contrastAtom from '@/atom.ts'
import type * as contrastData from '@/data/index.ts'
import type * as contrastExpression from '@/expression.ts'

export const size = {
  /**
   * @see https://developer.mozilla.org/en-US/docs/Web/CSS/box-sizing
   */
  mode(
    value: contrastExpression.ExpressionOpt<'border-box' | 'content-box'>,
  ): contrastAtom.Atom {
    return contrastAtom.atom('box-sizing', value)
  },

  min: {
    /**
     * @see https://developer.mozilla.org/en-US/docs/Web/CSS/min-width
     */
    w(
      value: contrastExpression.ExpressionOpt<
        contrastData.Length | contrastData.Pct
      >,
    ): contrastAtom.Atom {
      return contrastAtom.atom('min-width', value)
    },

    /**
     * @see https://developer.mozilla.org/en-US/docs/Web/CSS/min-height
     */
    h(
      value: contrastExpression.ExpressionOpt<
        contrastData.Length | contrastData.Pct
      >,
    ): contrastAtom.Atom {
      return contrastAtom.atom('min-height', value)
    },

    wh(
      value: contrastExpression.ExpressionOpt<
        contrastData.Length | contrastData.Pct
      >,
    ): contrastAtom.AtomOpt {
      return [size.min.w(value), size.min.h(value)]
    },

    /**
     * @see https://developer.mozilla.org/en-US/docs/Web/CSS/min-inline-size
     */
    o(
      value: contrastExpression.ExpressionOpt<
        contrastData.Length | contrastData.Pct
      >,
    ): contrastAtom.Atom {
      return contrastAtom.atom('min-block-size', value)
    },

    /**
     * @see https://developer.mozilla.org/en-US/docs/Web/CSS/min-inline-size
     */
    i(
      value: contrastExpression.ExpressionOpt<
        contrastData.Length | contrastData.Pct
      >,
    ): contrastAtom.Atom {
      return contrastAtom.atom('min-inline-size', value)
    },

    oi(
      value: contrastExpression.ExpressionOpt<
        contrastData.Length | contrastData.Pct
      >,
    ): contrastAtom.AtomOpt {
      return [size.min.o(value), size.min.i(value)]
    },
  },

  max: {
    /**
     * @see https://developer.mozilla.org/en-US/docs/Web/CSS/max-width
     */
    w(
      value: contrastExpression.ExpressionOpt<
        contrastData.Length | contrastData.Pct
      >,
    ): contrastAtom.Atom {
      return contrastAtom.atom('max-width', value)
    },

    /**
     * @see https://developer.mozilla.org/en-US/docs/Web/CSS/max-height
     */
    h(
      value: contrastExpression.ExpressionOpt<
        contrastData.Length | contrastData.Pct
      >,
    ): contrastAtom.Atom {
      return contrastAtom.atom('max-height', value)
    },

    wh(
      value: contrastExpression.ExpressionOpt<
        contrastData.Length | contrastData.Pct
      >,
    ): contrastAtom.AtomOpt {
      return [size.max.w(value), size.max.h(value)]
    },

    /**
     * @see https://developer.mozilla.org/en-US/docs/Web/CSS/max-block-size
     */
    o(
      value: contrastExpression.ExpressionOpt<
        contrastData.Length | contrastData.Pct
      >,
    ): contrastAtom.Atom {
      return contrastAtom.atom('max-block-size', value)
    },

    /**
     * @see https://developer.mozilla.org/en-US/docs/Web/CSS/max-inline-size
     */
    i(
      value: contrastExpression.ExpressionOpt<
        contrastData.Length | contrastData.Pct
      >,
    ): contrastAtom.Atom {
      return contrastAtom.atom('max-inline-size', value)
    },

    oi(
      value: contrastExpression.ExpressionOpt<
        contrastData.Length | contrastData.Pct
      >,
    ): contrastAtom.AtomOpt {
      return [size.max.o(value), size.max.i(value)]
    },
  },

  /**
   * @see https://developer.mozilla.org/en-US/docs/Web/CSS/height
   */
  w(
    value: contrastExpression.ExpressionOpt<
      contrastData.Length | contrastData.Pct
    >,
  ): contrastAtom.Atom {
    return contrastAtom.atom('width', value)
  },

  /**
   * @see https://developer.mozilla.org/en-US/docs/Web/CSS/height
   */
  h(
    value: contrastExpression.ExpressionOpt<
      contrastData.Length | contrastData.Pct
    >,
  ): contrastAtom.Atom {
    return contrastAtom.atom('height', value)
  },

  wh(
    value: contrastExpression.ExpressionOpt<
      contrastData.Length | contrastData.Pct
    >,
  ): contrastAtom.AtomOpt {
    return [size.w(value), size.h(value)]
  },

  /**
   * @see https://developer.mozilla.org/en-US/docs/Web/CSS/inline-size
   */
  o(
    value: contrastExpression.ExpressionOpt<
      contrastData.Length | contrastData.Pct
    >,
  ): contrastAtom.Atom {
    return contrastAtom.atom('block-size', value)
  },

  /**
   * @see https://developer.mozilla.org/en-US/docs/Web/CSS/inline-size
   */
  i(
    value: contrastExpression.ExpressionOpt<
      contrastData.Length | contrastData.Pct
    >,
  ): contrastAtom.Atom {
    return contrastAtom.atom('inline-size', value)
  },

  oi(
    value: contrastExpression.ExpressionOpt<
      contrastData.Length | contrastData.Pct
    >,
  ): contrastAtom.AtomOpt {
    return [size.o(value), size.i(value)]
  },
}
