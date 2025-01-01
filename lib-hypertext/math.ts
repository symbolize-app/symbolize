import * as hypertextElement from '@/element.ts'
import type * as hypertextFragment from '@/fragment.ts'
import type * as hypertextMathAttr from '@/mathAttr.ts'

export const math = new Proxy(
  {},
  {
    get(_mutableTarget: object, tag: string, _receiver: unknown): unknown {
      return (attrs: object) =>
        hypertextElement.elementFragment(
          (ctx) =>
            ctx.hypertext.document.createElementNS(
              'http://www.w3.org/1998/Math/MathML',
              tag,
            ),
          hypertextElement.ElementFragmentMode.normal,
          attrs,
        )
    },
  },
) as unknown as {
  readonly [Key in keyof hypertextMathAttr.MathAttrsTagNameMap<unknown> &
    keyof MathMLElementTagNameMap]: <CustomContext = unknown>(
    attrs: hypertextMathAttr.MathAttrsTagNameMap<CustomContext>[Key],
  ) => hypertextFragment.Fragment<CustomContext>
}
