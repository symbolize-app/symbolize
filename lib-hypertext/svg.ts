import * as hypertextElement from '@/element.ts'
import type * as hypertextFragment from '@/fragment.ts'
import type * as hypertextSvgAttr from '@/svgAttr.ts'

export const svg = new Proxy(
  {},
  {
    get(_mutableTarget: object, tag: string, _receiver: unknown): unknown {
      return (attrs: object) =>
        hypertextElement.elementFragment(
          (ctx) =>
            ctx.hypertext.document.createElementNS(
              'http://www.w3.org/2000/svg',
              tag,
            ),
          hypertextElement.ElementFragmentMode.normal,
          attrs,
        )
    },
  },
) as unknown as {
  readonly [Key in keyof hypertextSvgAttr.SvgAttrsTagNameMap<unknown> &
    keyof SVGElementTagNameMap]: <CustomContext = unknown>(
    attrs: hypertextSvgAttr.SvgAttrsTagNameMap<CustomContext>[Key],
  ) => hypertextFragment.Fragment<CustomContext>
}
