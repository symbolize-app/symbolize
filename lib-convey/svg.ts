import type * as conveyFragment from '@/fragment.ts'
import * as conveyOpaqueElement from '@/opaqueElement.ts'
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
        new conveyOpaqueElement.OpaqueElementFragment(
          conveyOpaqueElement.OpaqueElementAttributeCase.keep,
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