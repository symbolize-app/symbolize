import * as markupElement from '@/element.ts'
import type * as markupFragment from '@/fragment.ts'
import type * as markupHtmlAttr from '@/htmlAttr.ts'

export const html = new Proxy(
  {},
  {
    get(_mutableTarget: object, tag: string, _receiver: unknown): unknown {
      return (attrs: object) =>
        markupElement.elementFragment(
          (ctx) => ctx.markup.document.createElement(tag),
          markupElement.ElementFragmentMode.normal,
          attrs,
        )
    },
  },
) as unknown as {
  readonly [Key in keyof HTMLElementTagNameMap &
    keyof markupHtmlAttr.HtmlAttrsTagNameMap<unknown>]: <
    CustomContext = unknown,
  >(
    attrs: markupHtmlAttr.HtmlAttrsTagNameMap<CustomContext>[Key],
  ) => markupFragment.Fragment<CustomContext>
}
