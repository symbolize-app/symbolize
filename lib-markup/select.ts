import * as markupElementAttr from '@/elementAttr.ts'
import type * as markupHtmlAttr from '@/htmlAttr.ts'
import type * as markupMathAttr from '@/mathAttr.ts'
import type * as markupSvgAttr from '@/svgAttr.ts'
import type * as compute from '@symbolize/lib-compute'
import * as contrast from '@symbolize/lib-contrast'

type Attr = {
  readonly [Key in keyof markupElementAttr.AllAttrs as markupElementAttr.AllAttrs[Key]['kind'] extends (
    | markupElementAttr.ElementAttrKind.boolean
    | markupElementAttr.ElementAttrKind.string
  ) ?
    Key
  : never]?: markupElementAttr.AllAttrs[Key]['type'] extends (
    compute.NodeOpt<infer T>
  ) ?
    T
  : never
}

type Type =
  | keyof markupHtmlAttr.HtmlAttrsTagNameMap<unknown>
  | keyof markupMathAttr.MathAttrsTagNameMap<unknown>
  | keyof markupSvgAttr.SvgAttrsTagNameMap<unknown>

export const select = {
  /**
   * @see https://developer.mozilla.org/en-US/docs/Web/CSS/Attribute_selectors
   */
  attr(values: Attr): contrast.SelectTerm {
    const terms = Object.entries(values).map(([key, value]) =>
      contrast.select.term(compileAttrPair, () => [
        key as keyof Attr,
        value,
      ]),
    )
    return contrast.select.term(compileAttr, (ctx) =>
      terms.map((term) => term.compile(ctx)),
    )
  },

  /**
   * @see https://developer.mozilla.org/en-US/docs/Web/CSS/Type_selectors
   */
  type(value: Type, ...values: readonly Type[]): contrast.SelectTerm {
    return contrast.select.term(compileType, () => [value, ...values])
  },
}

function compileAttr(...values: readonly string[]): string {
  return values.join('')
}

function compileAttrPair(key: keyof Attr, value: unknown): string {
  const attrDefinition = markupElementAttr.allAttrs[key]
  const kind = attrDefinition.kind
  const name = attrDefinition.name
  if (
    kind === markupElementAttr.ElementAttrKind.boolean &&
    value === true
  ) {
    return `[${name}]`
  } else if (
    kind === markupElementAttr.ElementAttrKind.boolean &&
    value === false
  ) {
    return `:not([${name}])`
  } else {
    const valueItems = Array.isArray(value) ? value : [value]
    return `[${name}=${JSON.stringify(valueItems.join(' '))}]`
  }
}

function compileType(...values: readonly Type[]): string {
  return `:where(${values.join(',')})`
}
