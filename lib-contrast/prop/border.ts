import * as contrastAtom from '@/atom.ts'
import type * as contrastData from '@/data/index.ts'
import type * as contrastExpression from '@/expression.ts'

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
      value: contrastExpression.ExpressionOpt<contrastData.Color>,
    ): contrastAtom.Atom {
      return contrastAtom.atom('border-top-color', value)
    },

    /**
     * @see https://developer.mozilla.org/en-US/docs/Web/CSS/border-right-color
     */
    r(
      value: contrastExpression.ExpressionOpt<contrastData.Color>,
    ): contrastAtom.Atom {
      return contrastAtom.atom('border-right-color', value)
    },

    /**
     * @see https://developer.mozilla.org/en-US/docs/Web/CSS/border-bottom-color
     */
    b(
      value: contrastExpression.ExpressionOpt<contrastData.Color>,
    ): contrastAtom.Atom {
      return contrastAtom.atom('border-bottom-color', value)
    },

    /**
     * @see https://developer.mozilla.org/en-US/docs/Web/CSS/border-left-color
     */
    l(
      value: contrastExpression.ExpressionOpt<contrastData.Color>,
    ): contrastAtom.Atom {
      return contrastAtom.atom('border-left-color', value)
    },

    tb(
      value: contrastExpression.ExpressionOpt<contrastData.Color>,
    ): contrastAtom.AtomOpt {
      return [border.color.t(value), border.color.b(value)]
    },

    rl(
      value: contrastExpression.ExpressionOpt<contrastData.Color>,
    ): contrastAtom.AtomOpt {
      return [border.color.r(value), border.color.l(value)]
    },

    trbl(
      value: contrastExpression.ExpressionOpt<contrastData.Color>,
    ): contrastAtom.AtomOpt {
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
      value: contrastExpression.ExpressionOpt<contrastData.Color>,
    ): contrastAtom.Atom {
      return contrastAtom.atom('border-block-start-color', value)
    },

    /**
     * @see https://developer.mozilla.org/en-US/docs/Web/CSS/border-block-end-color
     */
    oe(
      value: contrastExpression.ExpressionOpt<contrastData.Color>,
    ): contrastAtom.Atom {
      return contrastAtom.atom('border-block-end-color', value)
    },

    /**
     * @see https://developer.mozilla.org/en-US/docs/Web/CSS/border-inline-start-color
     */
    is(
      value: contrastExpression.ExpressionOpt<contrastData.Color>,
    ): contrastAtom.Atom {
      return contrastAtom.atom('border-inline-start-color', value)
    },

    /**
     * @see https://developer.mozilla.org/en-US/docs/Web/CSS/border-inline-end-color
     */
    ie(
      value: contrastExpression.ExpressionOpt<contrastData.Color>,
    ): contrastAtom.Atom {
      return contrastAtom.atom('border-inline-end-color', value)
    },

    o(
      value: contrastExpression.ExpressionOpt<contrastData.Color>,
    ): contrastAtom.AtomOpt {
      return [border.color.os(value), border.color.oe(value)]
    },

    i(
      value: contrastExpression.ExpressionOpt<contrastData.Color>,
    ): contrastAtom.AtomOpt {
      return [border.color.is(value), border.color.ie(value)]
    },

    oi(
      value: contrastExpression.ExpressionOpt<contrastData.Color>,
    ): contrastAtom.AtomOpt {
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
    t(value: contrastExpression.ExpressionOpt<Style>): contrastAtom.Atom {
      return contrastAtom.atom('border-top-style', value)
    },

    /**
     * @see https://developer.mozilla.org/en-US/docs/Web/CSS/border-right-style
     */
    r(value: contrastExpression.ExpressionOpt<Style>): contrastAtom.Atom {
      return contrastAtom.atom('border-right-style', value)
    },

    /**
     * @see https://developer.mozilla.org/en-US/docs/Web/CSS/border-bottom-style
     */
    b(value: contrastExpression.ExpressionOpt<Style>): contrastAtom.Atom {
      return contrastAtom.atom('border-bottom-style', value)
    },

    /**
     * @see https://developer.mozilla.org/en-US/docs/Web/CSS/border-left-style
     */
    l(value: contrastExpression.ExpressionOpt<Style>): contrastAtom.Atom {
      return contrastAtom.atom('border-left-style', value)
    },

    tb(
      value: contrastExpression.ExpressionOpt<Style>,
    ): contrastAtom.AtomOpt {
      return [border.style.t(value), border.style.b(value)]
    },

    rl(
      value: contrastExpression.ExpressionOpt<Style>,
    ): contrastAtom.AtomOpt {
      return [border.style.r(value), border.style.l(value)]
    },

    trbl(
      value: contrastExpression.ExpressionOpt<Style>,
    ): contrastAtom.AtomOpt {
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
    os(value: contrastExpression.ExpressionOpt<Style>): contrastAtom.Atom {
      return contrastAtom.atom('border-block-start-style', value)
    },

    /**
     * @see https://developer.mozilla.org/en-US/docs/Web/CSS/border-block-end-style
     */
    oe(value: contrastExpression.ExpressionOpt<Style>): contrastAtom.Atom {
      return contrastAtom.atom('border-block-end-style', value)
    },

    /**
     * @see https://developer.mozilla.org/en-US/docs/Web/CSS/border-inline-start-style
     */
    is(value: contrastExpression.ExpressionOpt<Style>): contrastAtom.Atom {
      return contrastAtom.atom('border-inline-start-style', value)
    },

    /**
     * @see https://developer.mozilla.org/en-US/docs/Web/CSS/border-inline-end-style
     */
    ie(value: contrastExpression.ExpressionOpt<Style>): contrastAtom.Atom {
      return contrastAtom.atom('border-inline-end-style', value)
    },

    o(
      value: contrastExpression.ExpressionOpt<Style>,
    ): contrastAtom.AtomOpt {
      return [border.style.os(value), border.style.oe(value)]
    },

    i(
      value: contrastExpression.ExpressionOpt<Style>,
    ): contrastAtom.AtomOpt {
      return [border.style.is(value), border.style.ie(value)]
    },

    oi(
      value: contrastExpression.ExpressionOpt<Style>,
    ): contrastAtom.AtomOpt {
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
      value: contrastExpression.ExpressionOpt<contrastData.Length>,
    ): contrastAtom.Atom {
      return contrastAtom.atom('border-top-width', value)
    },

    /**
     * @see https://developer.mozilla.org/en-US/docs/Web/CSS/border-right-width
     */
    r(
      value: contrastExpression.ExpressionOpt<contrastData.Length>,
    ): contrastAtom.Atom {
      return contrastAtom.atom('border-right-width', value)
    },

    /**
     * @see https://developer.mozilla.org/en-US/docs/Web/CSS/border-bottom-width
     */
    b(
      value: contrastExpression.ExpressionOpt<contrastData.Length>,
    ): contrastAtom.Atom {
      return contrastAtom.atom('border-bottom-width', value)
    },

    /**
     * @see https://developer.mozilla.org/en-US/docs/Web/CSS/border-left-width
     */
    l(
      value: contrastExpression.ExpressionOpt<contrastData.Length>,
    ): contrastAtom.Atom {
      return contrastAtom.atom('border-left-width', value)
    },

    tb(
      value: contrastExpression.ExpressionOpt<contrastData.Length>,
    ): contrastAtom.AtomOpt {
      return [border.width.t(value), border.width.b(value)]
    },

    rl(
      value: contrastExpression.ExpressionOpt<contrastData.Length>,
    ): contrastAtom.AtomOpt {
      return [border.width.r(value), border.width.l(value)]
    },

    trbl(
      value: contrastExpression.ExpressionOpt<contrastData.Length>,
    ): contrastAtom.AtomOpt {
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
      value: contrastExpression.ExpressionOpt<contrastData.Length>,
    ): contrastAtom.Atom {
      return contrastAtom.atom('border-block-start-width', value)
    },

    /**
     * @see https://developer.mozilla.org/en-US/docs/Web/CSS/border-block-end-width
     */
    oe(
      value: contrastExpression.ExpressionOpt<contrastData.Length>,
    ): contrastAtom.Atom {
      return contrastAtom.atom('border-block-end-width', value)
    },

    /**
     * @see https://developer.mozilla.org/en-US/docs/Web/CSS/border-inline-start-width
     */
    is(
      value: contrastExpression.ExpressionOpt<contrastData.Length>,
    ): contrastAtom.Atom {
      return contrastAtom.atom('border-inline-start-width', value)
    },

    /**
     * @see https://developer.mozilla.org/en-US/docs/Web/CSS/border-inline-end-width
     */
    ie(
      value: contrastExpression.ExpressionOpt<contrastData.Length>,
    ): contrastAtom.Atom {
      return contrastAtom.atom('border-inline-end-width', value)
    },

    o(
      value: contrastExpression.ExpressionOpt<contrastData.Length>,
    ): contrastAtom.AtomOpt {
      return [border.width.os(value), border.width.oe(value)]
    },

    i(
      value: contrastExpression.ExpressionOpt<contrastData.Length>,
    ): contrastAtom.AtomOpt {
      return [border.width.is(value), border.width.ie(value)]
    },

    oi(
      value: contrastExpression.ExpressionOpt<contrastData.Length>,
    ): contrastAtom.AtomOpt {
      return [
        border.width.os(value),
        border.width.oe(value),
        border.width.is(value),
        border.width.ie(value),
      ]
    },
  },
}
