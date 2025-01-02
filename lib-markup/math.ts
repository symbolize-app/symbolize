import * as markupElement from '@/element.ts'
import type * as markupFragment from '@/fragment.ts'
import type * as markupMathAttr from '@/mathAttr.ts'

export const math = new Proxy(
  {},
  {
    get(_mutableTarget: object, tag: string, _receiver: unknown): unknown {
      return (attrs: object) =>
        markupElement.elementFragment(
          (ctx) =>
            ctx.markup.document.createElementNS(
              'http://www.w3.org/1998/Math/MathML',
              tag,
            ),
          markupElement.ElementFragmentMode.normal,
          attrs,
        )
    },
  },
) as unknown as {
  readonly [Key in keyof markupMathAttr.MathAttrsTagNameMap<unknown> &
    keyof MathMLElementTagNameMap]: <CustomContext = unknown>(
    attrs: markupMathAttr.MathAttrsTagNameMap<CustomContext>[Key],
  ) => markupFragment.Fragment<CustomContext>
}
