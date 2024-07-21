import * as conveyElementAttr from '@/elementAttr.ts'
import type * as conveyHtmlAttr from '@/htmlAttr.ts'
import type * as conveyMathAttr from '@/mathAttr.ts'
import type * as conveySvgAttr from '@/svgAttr.ts'
import type * as compute from '@intertwine/lib-compute'
import * as contrast from '@intertwine/lib-contrast'

type Attr = {
  readonly [Key in keyof conveyElementAttr.AllAttrs as conveyElementAttr.AllAttrs[Key]['kind'] extends (
    | conveyElementAttr.ElementAttrKind.boolean
    | conveyElementAttr.ElementAttrKind.string
  ) ?
    Key
  : never]?: conveyElementAttr.AllAttrs[Key]['type'] extends (
    compute.NodeOpt<infer T>
  ) ?
    T
  : never
}

type Type =
  | keyof conveyHtmlAttr.HtmlAttrsTagNameMap<unknown>
  | keyof conveyMathAttr.MathAttrsTagNameMap<unknown>
  | keyof conveySvgAttr.SvgAttrsTagNameMap<unknown>

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
  const attrDefinition = conveyElementAttr.allAttrs[key]
  const kind = attrDefinition.kind
  const name = attrDefinition.name
  if (
    kind === conveyElementAttr.ElementAttrKind.boolean &&
    value === true
  ) {
    return `[${name}]`
  } else if (
    kind === conveyElementAttr.ElementAttrKind.boolean &&
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
