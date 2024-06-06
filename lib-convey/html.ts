import * as conveyElement from '@/element.ts'
import type * as conveyFragment from '@/fragment.ts'
import type * as conveyHtmlAttrs from '@/htmlAttrs.ts'

export const html = new Proxy(
  {},
  {
    get(_mutableTarget: object, tag: string, _receiver: unknown): unknown {
      return (attrs: object) =>
        new conveyElement.ElementFragment(
          (ctx) => ctx.convey.document.createElement(tag),
          conveyElement.ElementFragmentMode.normal,
          attrs,
        )
    },
  },
) as unknown as {
  readonly [Key in keyof conveyHtmlAttrs.HtmlAttrsTagNameMap<unknown> &
    keyof HTMLElementTagNameMap]: <CustomContext = unknown>(
    attrs: conveyHtmlAttrs.HtmlAttrsTagNameMap<CustomContext>[Key],
  ) => conveyFragment.Fragment<CustomContext>
}
