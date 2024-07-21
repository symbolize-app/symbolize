import * as conveyElement from '@/element.ts'
import type * as conveyFragment from '@/fragment.ts'
import type * as conveyHtmlAttr from '@/htmlAttr.ts'
import type * as conveyMathAttr from '@/mathAttr.ts'
import type * as conveySvgAttr from '@/svgAttr.ts'

export function portal<
  CustomElement extends conveyElement.SupportedElement,
  CustomContext = unknown,
>(
  element: CustomElement,
  attrs: CustomElement extends HTMLElement ?
    conveyHtmlAttr.HtmlAttrsByElement<CustomElement, CustomContext>
  : CustomElement extends SVGElement ?
    conveySvgAttr.SvgAttrsByElement<CustomElement, CustomContext>
  : CustomElement extends MathMLElement ?
    conveyMathAttr.MathAttrs<CustomContext, CustomElement>
  : never,
): conveyFragment.Fragment<CustomContext> {
  return new conveyElement.ElementFragment(
    () => element,
    conveyElement.ElementFragmentMode.portal,
    attrs,
  )
}
