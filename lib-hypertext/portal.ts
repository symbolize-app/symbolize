import * as hypertextElement from '@/element.ts'
import type * as hypertextFragment from '@/fragment.ts'
import type * as hypertextHtmlAttr from '@/htmlAttr.ts'
import type * as hypertextMathAttr from '@/mathAttr.ts'
import type * as hypertextSvgAttr from '@/svgAttr.ts'

export function portal<
  CustomElement extends hypertextElement.SupportedElement,
  CustomContext = unknown,
>(
  element: CustomElement,
  attrs: CustomElement extends HTMLElement ?
    hypertextHtmlAttr.HtmlAttrsByElement<CustomElement, CustomContext>
  : CustomElement extends SVGElement ?
    hypertextSvgAttr.SvgAttrsByElement<CustomElement, CustomContext>
  : CustomElement extends MathMLElement ?
    hypertextMathAttr.MathAttrs<CustomContext, CustomElement>
  : never,
): hypertextFragment.Fragment<CustomContext> {
  return hypertextElement.elementFragment(
    () => element,
    hypertextElement.ElementFragmentMode.portal,
    attrs,
  )
}
