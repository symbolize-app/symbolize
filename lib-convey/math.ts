import type * as conveyFragment from '@/fragment.ts'
import type * as conveyMathAttrs from '@/mathAttrs.ts'
import * as conveyOpaqueElement from '@/opaqueElement.ts'

export const math = new Proxy(
  {},
  {
    get(
      _mutableTarget: object,
      property: string,
      _receiver: unknown,
    ): unknown {
      return (attrs: object) =>
        new conveyOpaqueElement.OpaqueElementFragment(
          conveyOpaqueElement.OpaqueElementAttributeCase.lower,
          'http://www.w3.org/1998/Math/MathML',
          property,
          attrs,
        )
    },
  },
) as unknown as {
  readonly [Key in keyof conveyMathAttrs.AttrsTagNameMap<unknown> &
    keyof MathMLElementTagNameMap]: <CustomContext = unknown>(
    attrs: conveyMathAttrs.AttrsTagNameMap<CustomContext>[Key],
  ) => conveyFragment.Fragment<CustomContext>
}
