import * as conveyElement from '@/element.ts'
import type * as conveyFragment from '@/fragment.ts'
import type * as conveyMathAttrs from '@/mathAttrs.ts'

export const math = new Proxy(
  {},
  {
    get(
      _mutableTarget: object,
      property: string,
      _receiver: unknown,
    ): unknown {
      return (attrs: object) =>
        new conveyElement.ElementFragment(
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
