import * as conveyElement from '@/element.ts'
import type * as conveyFragment from '@/fragment.ts'
import type * as conveyHtmlAttr from '@/htmlAttr.ts'

export const html = new Proxy(
  {},
  {
    get(_mutableTarget: object, tag: string, _receiver: unknown): unknown {
      return (attrs: object) =>
        conveyElement.elementFragment(
          (ctx) => ctx.convey.document.createElement(tag),
          conveyElement.ElementFragmentMode.normal,
          attrs,
        )
    },
  },
) as unknown as {
  readonly [Key in keyof conveyHtmlAttr.HtmlAttrsTagNameMap<unknown> &
    keyof HTMLElementTagNameMap]: <CustomContext = unknown>(
    attrs: conveyHtmlAttr.HtmlAttrsTagNameMap<CustomContext>[Key],
  ) => conveyFragment.Fragment<CustomContext>
}
