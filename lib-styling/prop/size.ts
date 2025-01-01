import * as stylingAtom from '@/atom.ts'
import type * as stylingData from '@/data/index.ts'
import type * as stylingExpression from '@/expression.ts'

export const size = {
  /**
   * @see https://developer.mozilla.org/en-US/docs/Web/CSS/box-sizing
   */
  mode(
    value: stylingExpression.ExpressionOpt<'border-box' | 'content-box'>,
  ): stylingAtom.Atom {
    return stylingAtom.atom('box-sizing', value)
  },

  min: {
    /**
     * @see https://developer.mozilla.org/en-US/docs/Web/CSS/min-width
     */
    w(
      value: stylingExpression.ExpressionOpt<
        stylingData.Length | stylingData.Pct
      >,
    ): stylingAtom.Atom {
      return stylingAtom.atom('min-width', value)
    },

    /**
     * @see https://developer.mozilla.org/en-US/docs/Web/CSS/min-height
     */
    h(
      value: stylingExpression.ExpressionOpt<
        stylingData.Length | stylingData.Pct
      >,
    ): stylingAtom.Atom {
      return stylingAtom.atom('min-height', value)
    },

    wh(
      value: stylingExpression.ExpressionOpt<
        stylingData.Length | stylingData.Pct
      >,
    ): stylingAtom.AtomOpt {
      return [size.min.w(value), size.min.h(value)]
    },

    /**
     * @see https://developer.mozilla.org/en-US/docs/Web/CSS/min-inline-size
     */
    o(
      value: stylingExpression.ExpressionOpt<
        stylingData.Length | stylingData.Pct
      >,
    ): stylingAtom.Atom {
      return stylingAtom.atom('min-block-size', value)
    },

    /**
     * @see https://developer.mozilla.org/en-US/docs/Web/CSS/min-inline-size
     */
    i(
      value: stylingExpression.ExpressionOpt<
        stylingData.Length | stylingData.Pct
      >,
    ): stylingAtom.Atom {
      return stylingAtom.atom('min-inline-size', value)
    },

    oi(
      value: stylingExpression.ExpressionOpt<
        stylingData.Length | stylingData.Pct
      >,
    ): stylingAtom.AtomOpt {
      return [size.min.o(value), size.min.i(value)]
    },
  },

  max: {
    /**
     * @see https://developer.mozilla.org/en-US/docs/Web/CSS/max-width
     */
    w(
      value: stylingExpression.ExpressionOpt<
        stylingData.Length | stylingData.Pct
      >,
    ): stylingAtom.Atom {
      return stylingAtom.atom('max-width', value)
    },

    /**
     * @see https://developer.mozilla.org/en-US/docs/Web/CSS/max-height
     */
    h(
      value: stylingExpression.ExpressionOpt<
        stylingData.Length | stylingData.Pct
      >,
    ): stylingAtom.Atom {
      return stylingAtom.atom('max-height', value)
    },

    wh(
      value: stylingExpression.ExpressionOpt<
        stylingData.Length | stylingData.Pct
      >,
    ): stylingAtom.AtomOpt {
      return [size.max.w(value), size.max.h(value)]
    },

    /**
     * @see https://developer.mozilla.org/en-US/docs/Web/CSS/max-block-size
     */
    o(
      value: stylingExpression.ExpressionOpt<
        stylingData.Length | stylingData.Pct
      >,
    ): stylingAtom.Atom {
      return stylingAtom.atom('max-block-size', value)
    },

    /**
     * @see https://developer.mozilla.org/en-US/docs/Web/CSS/max-inline-size
     */
    i(
      value: stylingExpression.ExpressionOpt<
        stylingData.Length | stylingData.Pct
      >,
    ): stylingAtom.Atom {
      return stylingAtom.atom('max-inline-size', value)
    },

    oi(
      value: stylingExpression.ExpressionOpt<
        stylingData.Length | stylingData.Pct
      >,
    ): stylingAtom.AtomOpt {
      return [size.max.o(value), size.max.i(value)]
    },
  },

  /**
   * @see https://developer.mozilla.org/en-US/docs/Web/CSS/height
   */
  w(
    value: stylingExpression.ExpressionOpt<
      stylingData.Length | stylingData.Pct
    >,
  ): stylingAtom.Atom {
    return stylingAtom.atom('width', value)
  },

  /**
   * @see https://developer.mozilla.org/en-US/docs/Web/CSS/height
   */
  h(
    value: stylingExpression.ExpressionOpt<
      stylingData.Length | stylingData.Pct
    >,
  ): stylingAtom.Atom {
    return stylingAtom.atom('height', value)
  },

  wh(
    value: stylingExpression.ExpressionOpt<
      stylingData.Length | stylingData.Pct
    >,
  ): stylingAtom.AtomOpt {
    return [size.w(value), size.h(value)]
  },

  /**
   * @see https://developer.mozilla.org/en-US/docs/Web/CSS/inline-size
   */
  o(
    value: stylingExpression.ExpressionOpt<
      stylingData.Length | stylingData.Pct
    >,
  ): stylingAtom.Atom {
    return stylingAtom.atom('block-size', value)
  },

  /**
   * @see https://developer.mozilla.org/en-US/docs/Web/CSS/inline-size
   */
  i(
    value: stylingExpression.ExpressionOpt<
      stylingData.Length | stylingData.Pct
    >,
  ): stylingAtom.Atom {
    return stylingAtom.atom('inline-size', value)
  },

  oi(
    value: stylingExpression.ExpressionOpt<
      stylingData.Length | stylingData.Pct
    >,
  ): stylingAtom.AtomOpt {
    return [size.o(value), size.i(value)]
  },
}
