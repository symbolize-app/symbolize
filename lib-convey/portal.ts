import * as conveyElement from '@/element.ts'
import type * as conveyFragment from '@/fragment.ts'
import type * as conveyHtmlAttrs from '@/htmlAttrs.ts'
import type * as conveyMathAttrs from '@/mathAttrs.ts'
import type * as conveySvgAttrs from '@/svgAttrs.ts'

export function portal<
  CustomElement extends conveyElement.SupportedElement,
  CustomContext = unknown,
>(
  element: CustomElement,
  attrs: CustomElement extends HTMLElement ?
    conveyHtmlAttrs.HtmlAttrsByElement<CustomElement, CustomContext>
  : CustomElement extends SVGElement ?
    conveySvgAttrs.SvgAttrsByElement<CustomElement, CustomContext>
  : CustomElement extends MathMLElement ?
    conveyMathAttrs.MathAttrs<CustomContext, CustomElement>
  : never,
): conveyFragment.Fragment<CustomContext> {
  return new conveyElement.ElementFragment(
    () => element,
    conveyElement.ElementFragmentMode.portal,
    attrs,
  )
}
