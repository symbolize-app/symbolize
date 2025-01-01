import * as hypertextElement from '@/element.ts'
import type * as hypertextFragment from '@/fragment.ts'
import type * as hypertextHtmlAttr from '@/htmlAttr.ts'

export const html = new Proxy(
  {},
  {
    get(_mutableTarget: object, tag: string, _receiver: unknown): unknown {
      return (attrs: object) =>
        hypertextElement.elementFragment(
          (ctx) => ctx.hypertext.document.createElement(tag),
          hypertextElement.ElementFragmentMode.normal,
          attrs,
        )
    },
  },
) as unknown as {
  readonly [Key in keyof HTMLElementTagNameMap &
    keyof hypertextHtmlAttr.HtmlAttrsTagNameMap<unknown>]: <
    CustomContext = unknown,
  >(
    attrs: hypertextHtmlAttr.HtmlAttrsTagNameMap<CustomContext>[Key],
  ) => hypertextFragment.Fragment<CustomContext>
}
