import * as conveyElement from '@/element.ts'
import type * as conveyFragment from '@/fragment.ts'
import type * as conveyMathAttrs from '@/mathAttrs.ts'

export const math = new Proxy(
  {},
  {
    get(_mutableTarget: object, tag: string, _receiver: unknown): unknown {
      return (attrs: object) =>
        new conveyElement.ElementFragment(
          (ctx) =>
            ctx.convey.document.createElementNS(
              'http://www.w3.org/1998/Math/MathML',
              tag,
            ),
          conveyElement.ElementFragmentMode.normal,
          attrs,
        )
    },
  },
) as unknown as {
  readonly [Key in keyof conveyMathAttrs.MathAttrsTagNameMap<unknown> &
    keyof MathMLElementTagNameMap]: <CustomContext = unknown>(
    attrs: conveyMathAttrs.MathAttrsTagNameMap<CustomContext>[Key],
  ) => conveyFragment.Fragment<CustomContext>
}
