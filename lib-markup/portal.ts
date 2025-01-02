import * as markupElement from '@/element.ts'
import type * as markupFragment from '@/fragment.ts'
import type * as markupHtmlAttr from '@/htmlAttr.ts'
import type * as markupMathAttr from '@/mathAttr.ts'
import type * as markupSvgAttr from '@/svgAttr.ts'

export function portal<
  CustomElement extends markupElement.SupportedElement,
  CustomContext = unknown,
>(
  element: CustomElement,
  attrs: CustomElement extends HTMLElement ?
    markupHtmlAttr.HtmlAttrsByElement<CustomElement, CustomContext>
  : CustomElement extends SVGElement ?
    markupSvgAttr.SvgAttrsByElement<CustomElement, CustomContext>
  : CustomElement extends MathMLElement ?
    markupMathAttr.MathAttrs<CustomContext, CustomElement>
  : never,
): markupFragment.Fragment<CustomContext> {
  return markupElement.elementFragment(
    () => element,
    markupElement.ElementFragmentMode.portal,
    attrs,
  )
}
