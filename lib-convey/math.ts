import * as conveyElement from '@/element.ts'
import type * as conveyFragment from '@/fragment.ts'
import type * as conveyMathAttr from '@/mathAttr.ts'

export const math = new Proxy(
  {},
  {
    get(_mutableTarget: object, tag: string, _receiver: unknown): unknown {
      return (attrs: object) =>
        conveyElement.elementFragment(
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
  readonly [Key in keyof conveyMathAttr.MathAttrsTagNameMap<unknown> &
    keyof MathMLElementTagNameMap]: <CustomContext = unknown>(
    attrs: conveyMathAttr.MathAttrsTagNameMap<CustomContext>[Key],
  ) => conveyFragment.Fragment<CustomContext>
}
