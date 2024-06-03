import * as conveyElement from '@/element.ts'
import type * as conveyFragment from '@/fragment.ts'
import type * as conveySvgAttrs from '@/svgAttrs.ts'

export const svg = new Proxy(
  {},
  {
    get(_mutableTarget: object, tag: string, _receiver: unknown): unknown {
      return (attrs: object) =>
        new conveyElement.ElementFragment(
          (ctx) =>
            ctx.convey.document.createElementNS(
              'http://www.w3.org/2000/svg',
              tag,
            ),
          conveyElement.ElementFragmentMode.normal,
          attrs,
        )
    },
  },
) as unknown as {
  readonly [Key in keyof conveySvgAttrs.SvgAttrsTagNameMap<unknown> &
    keyof SVGElementTagNameMap]: <CustomContext = unknown>(
    attrs: conveySvgAttrs.SvgAttrsTagNameMap<CustomContext>[Key],
  ) => conveyFragment.Fragment<CustomContext>
}
