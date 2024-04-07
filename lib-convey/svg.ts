import * as conveyElement from '@/element.ts'
import type * as conveyFragment from '@/fragment.ts'
import type * as conveySvgAttrs from '@/svgAttrs.ts'

export const svg = new Proxy(
  {},
  {
    get(
      _mutableTarget: object,
      property: string,
      _receiver: unknown,
    ): unknown {
      return (attrs: object) =>
        new conveyElement.ElementFragment(
          'http://www.w3.org/2000/svg',
          property,
          attrs,
        )
    },
  },
) as unknown as {
  readonly [Key in keyof conveySvgAttrs.AttrsTagNameMap<unknown> &
    keyof SVGElementTagNameMap]: <CustomContext = unknown>(
    attrs: conveySvgAttrs.AttrsTagNameMap<CustomContext>[Key],
  ) => conveyFragment.Fragment<CustomContext>
}
