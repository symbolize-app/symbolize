import * as markupElement from '@/element.ts'
import type * as markupFragment from '@/fragment.ts'
import type * as markupSvgAttr from '@/svgAttr.ts'

export const svg = new Proxy(
  {},
  {
    get(_mutableTarget: object, tag: string, _receiver: unknown): unknown {
      return (attrs: object) =>
        markupElement.elementFragment(
          (ctx) =>
            ctx.markup.document.createElementNS(
              'http://www.w3.org/2000/svg',
              tag,
            ),
          markupElement.ElementFragmentMode.normal,
          attrs,
        )
    },
  },
) as unknown as {
  readonly [Key in keyof markupSvgAttr.SvgAttrsTagNameMap<unknown> &
    keyof SVGElementTagNameMap]: <CustomContext = unknown>(
    attrs: markupSvgAttr.SvgAttrsTagNameMap<CustomContext>[Key],
  ) => markupFragment.Fragment<CustomContext>
}
