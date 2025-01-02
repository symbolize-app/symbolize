import * as stylingAtom from '@/atom.ts'
import type * as stylingData from '@/data/index.ts'
import type * as stylingExpression from '@/expression.ts'

type Style =
  | 'dashed'
  | 'dotted'
  | 'double'
  | 'groove'
  | 'hidden'
  | 'inset'
  | 'none'
  | 'outset'
  | 'ridge'
  | 'solid'

export const border = {
  color: {
    /**
     * @see https://developer.mozilla.org/en-US/docs/Web/CSS/border-top-color
     */
    t(
      value: stylingExpression.ExpressionOpt<stylingData.Color>,
    ): stylingAtom.Atom {
      return stylingAtom.atom('border-top-color', value)
    },

    /**
     * @see https://developer.mozilla.org/en-US/docs/Web/CSS/border-right-color
     */
    r(
      value: stylingExpression.ExpressionOpt<stylingData.Color>,
    ): stylingAtom.Atom {
      return stylingAtom.atom('border-right-color', value)
    },

    /**
     * @see https://developer.mozilla.org/en-US/docs/Web/CSS/border-bottom-color
     */
    b(
      value: stylingExpression.ExpressionOpt<stylingData.Color>,
    ): stylingAtom.Atom {
      return stylingAtom.atom('border-bottom-color', value)
    },

    /**
     * @see https://developer.mozilla.org/en-US/docs/Web/CSS/border-left-color
     */
    l(
      value: stylingExpression.ExpressionOpt<stylingData.Color>,
    ): stylingAtom.Atom {
      return stylingAtom.atom('border-left-color', value)
    },

    tb(
      value: stylingExpression.ExpressionOpt<stylingData.Color>,
    ): stylingAtom.AtomOpt {
      return [border.color.t(value), border.color.b(value)]
    },

    rl(
      value: stylingExpression.ExpressionOpt<stylingData.Color>,
    ): stylingAtom.AtomOpt {
      return [border.color.r(value), border.color.l(value)]
    },

    trbl(
      value: stylingExpression.ExpressionOpt<stylingData.Color>,
    ): stylingAtom.AtomOpt {
      return [
        border.color.t(value),
        border.color.r(value),
        border.color.b(value),
        border.color.l(value),
      ]
    },

    /**
     * @see https://developer.mozilla.org/en-US/docs/Web/CSS/border-block-start-color
     */
    os(
      value: stylingExpression.ExpressionOpt<stylingData.Color>,
    ): stylingAtom.Atom {
      return stylingAtom.atom('border-block-start-color', value)
    },

    /**
     * @see https://developer.mozilla.org/en-US/docs/Web/CSS/border-block-end-color
     */
    oe(
      value: stylingExpression.ExpressionOpt<stylingData.Color>,
    ): stylingAtom.Atom {
      return stylingAtom.atom('border-block-end-color', value)
    },

    /**
     * @see https://developer.mozilla.org/en-US/docs/Web/CSS/border-inline-start-color
     */
    is(
      value: stylingExpression.ExpressionOpt<stylingData.Color>,
    ): stylingAtom.Atom {
      return stylingAtom.atom('border-inline-start-color', value)
    },

    /**
     * @see https://developer.mozilla.org/en-US/docs/Web/CSS/border-inline-end-color
     */
    ie(
      value: stylingExpression.ExpressionOpt<stylingData.Color>,
    ): stylingAtom.Atom {
      return stylingAtom.atom('border-inline-end-color', value)
    },

    o(
      value: stylingExpression.ExpressionOpt<stylingData.Color>,
    ): stylingAtom.AtomOpt {
      return [border.color.os(value), border.color.oe(value)]
    },

    i(
      value: stylingExpression.ExpressionOpt<stylingData.Color>,
    ): stylingAtom.AtomOpt {
      return [border.color.is(value), border.color.ie(value)]
    },

    oi(
      value: stylingExpression.ExpressionOpt<stylingData.Color>,
    ): stylingAtom.AtomOpt {
      return [
        border.color.os(value),
        border.color.oe(value),
        border.color.is(value),
        border.color.ie(value),
      ]
    },
  },

  style: {
    /**
     * @see https://developer.mozilla.org/en-US/docs/Web/CSS/border-top-style
     */
    t(value: stylingExpression.ExpressionOpt<Style>): stylingAtom.Atom {
      return stylingAtom.atom('border-top-style', value)
    },

    /**
     * @see https://developer.mozilla.org/en-US/docs/Web/CSS/border-right-style
     */
    r(value: stylingExpression.ExpressionOpt<Style>): stylingAtom.Atom {
      return stylingAtom.atom('border-right-style', value)
    },

    /**
     * @see https://developer.mozilla.org/en-US/docs/Web/CSS/border-bottom-style
     */
    b(value: stylingExpression.ExpressionOpt<Style>): stylingAtom.Atom {
      return stylingAtom.atom('border-bottom-style', value)
    },

    /**
     * @see https://developer.mozilla.org/en-US/docs/Web/CSS/border-left-style
     */
    l(value: stylingExpression.ExpressionOpt<Style>): stylingAtom.Atom {
      return stylingAtom.atom('border-left-style', value)
    },

    tb(
      value: stylingExpression.ExpressionOpt<Style>,
    ): stylingAtom.AtomOpt {
      return [border.style.t(value), border.style.b(value)]
    },

    rl(
      value: stylingExpression.ExpressionOpt<Style>,
    ): stylingAtom.AtomOpt {
      return [border.style.r(value), border.style.l(value)]
    },

    trbl(
      value: stylingExpression.ExpressionOpt<Style>,
    ): stylingAtom.AtomOpt {
      return [
        border.style.t(value),
        border.style.r(value),
        border.style.b(value),
        border.style.l(value),
      ]
    },

    /**
     * @see https://developer.mozilla.org/en-US/docs/Web/CSS/border-block-start-style
     */
    os(value: stylingExpression.ExpressionOpt<Style>): stylingAtom.Atom {
      return stylingAtom.atom('border-block-start-style', value)
    },

    /**
     * @see https://developer.mozilla.org/en-US/docs/Web/CSS/border-block-end-style
     */
    oe(value: stylingExpression.ExpressionOpt<Style>): stylingAtom.Atom {
      return stylingAtom.atom('border-block-end-style', value)
    },

    /**
     * @see https://developer.mozilla.org/en-US/docs/Web/CSS/border-inline-start-style
     */
    is(value: stylingExpression.ExpressionOpt<Style>): stylingAtom.Atom {
      return stylingAtom.atom('border-inline-start-style', value)
    },

    /**
     * @see https://developer.mozilla.org/en-US/docs/Web/CSS/border-inline-end-style
     */
    ie(value: stylingExpression.ExpressionOpt<Style>): stylingAtom.Atom {
      return stylingAtom.atom('border-inline-end-style', value)
    },

    o(value: stylingExpression.ExpressionOpt<Style>): stylingAtom.AtomOpt {
      return [border.style.os(value), border.style.oe(value)]
    },

    i(value: stylingExpression.ExpressionOpt<Style>): stylingAtom.AtomOpt {
      return [border.style.is(value), border.style.ie(value)]
    },

    oi(
      value: stylingExpression.ExpressionOpt<Style>,
    ): stylingAtom.AtomOpt {
      return [
        border.style.os(value),
        border.style.oe(value),
        border.style.is(value),
        border.style.ie(value),
      ]
    },
  },

  width: {
    /**
     * @see https://developer.mozilla.org/en-US/docs/Web/CSS/border-top-width
     */
    t(
      value: stylingExpression.ExpressionOpt<stylingData.Length>,
    ): stylingAtom.Atom {
      return stylingAtom.atom('border-top-width', value)
    },

    /**
     * @see https://developer.mozilla.org/en-US/docs/Web/CSS/border-right-width
     */
    r(
      value: stylingExpression.ExpressionOpt<stylingData.Length>,
    ): stylingAtom.Atom {
      return stylingAtom.atom('border-right-width', value)
    },

    /**
     * @see https://developer.mozilla.org/en-US/docs/Web/CSS/border-bottom-width
     */
    b(
      value: stylingExpression.ExpressionOpt<stylingData.Length>,
    ): stylingAtom.Atom {
      return stylingAtom.atom('border-bottom-width', value)
    },

    /**
     * @see https://developer.mozilla.org/en-US/docs/Web/CSS/border-left-width
     */
    l(
      value: stylingExpression.ExpressionOpt<stylingData.Length>,
    ): stylingAtom.Atom {
      return stylingAtom.atom('border-left-width', value)
    },

    tb(
      value: stylingExpression.ExpressionOpt<stylingData.Length>,
    ): stylingAtom.AtomOpt {
      return [border.width.t(value), border.width.b(value)]
    },

    rl(
      value: stylingExpression.ExpressionOpt<stylingData.Length>,
    ): stylingAtom.AtomOpt {
      return [border.width.r(value), border.width.l(value)]
    },

    trbl(
      value: stylingExpression.ExpressionOpt<stylingData.Length>,
    ): stylingAtom.AtomOpt {
      return [
        border.width.t(value),
        border.width.r(value),
        border.width.b(value),
        border.width.l(value),
      ]
    },

    /**
     * @see https://developer.mozilla.org/en-US/docs/Web/CSS/border-block-start-width
     */
    os(
      value: stylingExpression.ExpressionOpt<stylingData.Length>,
    ): stylingAtom.Atom {
      return stylingAtom.atom('border-block-start-width', value)
    },

    /**
     * @see https://developer.mozilla.org/en-US/docs/Web/CSS/border-block-end-width
     */
    oe(
      value: stylingExpression.ExpressionOpt<stylingData.Length>,
    ): stylingAtom.Atom {
      return stylingAtom.atom('border-block-end-width', value)
    },

    /**
     * @see https://developer.mozilla.org/en-US/docs/Web/CSS/border-inline-start-width
     */
    is(
      value: stylingExpression.ExpressionOpt<stylingData.Length>,
    ): stylingAtom.Atom {
      return stylingAtom.atom('border-inline-start-width', value)
    },

    /**
     * @see https://developer.mozilla.org/en-US/docs/Web/CSS/border-inline-end-width
     */
    ie(
      value: stylingExpression.ExpressionOpt<stylingData.Length>,
    ): stylingAtom.Atom {
      return stylingAtom.atom('border-inline-end-width', value)
    },

    o(
      value: stylingExpression.ExpressionOpt<stylingData.Length>,
    ): stylingAtom.AtomOpt {
      return [border.width.os(value), border.width.oe(value)]
    },

    i(
      value: stylingExpression.ExpressionOpt<stylingData.Length>,
    ): stylingAtom.AtomOpt {
      return [border.width.is(value), border.width.ie(value)]
    },

    oi(
      value: stylingExpression.ExpressionOpt<stylingData.Length>,
    ): stylingAtom.AtomOpt {
      return [
        border.width.os(value),
        border.width.oe(value),
        border.width.is(value),
        border.width.ie(value),
      ]
    },
  },
}
