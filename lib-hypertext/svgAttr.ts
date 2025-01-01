import type * as hypertextElementAttr from '@/elementAttr.ts'

export interface SvgAttrsTagNameMap<CustomContext> {
  readonly g: SvgAttrs<CustomContext, SVGGElement>
  readonly rect: RectAttrs<CustomContext, SVGRectElement>
  readonly svg: SvgSvgAttrs<CustomContext, SVGSVGElement>
}

export type SvgAttrsByElement<
  CustomElement extends SVGElement,
  CustomContext,
> = {
  [Key in keyof SvgAttrsTagNameMap<CustomContext> &
    keyof SVGElementTagNameMap]: SVGElementTagNameMap[Key] extends (
    CustomElement
  ) ?
    SvgAttrsTagNameMap<CustomContext>[Key]
  : never
}[keyof SvgAttrsTagNameMap<CustomContext> & keyof SVGElementTagNameMap]

export type SvgAttrs<
  CustomContext,
  BaseElement extends Element,
> = hypertextElementAttr.Attrs<CustomContext, BaseElement> &
  hypertextElementAttr.PickAttrs<'requiredExtensions' | 'systemLanguage'>

export type RectAttrs<
  CustomContext,
  BaseElement extends Element,
> = hypertextElementAttr.PickAttrs<
  'height' | 'pathLength' | 'rx' | 'ry' | 'width' | 'x' | 'y'
> &
  SvgAttrs<CustomContext, BaseElement>

export type SvgSvgAttrs<
  CustomContext,
  BaseElement extends Element,
> = hypertextElementAttr.PickAttrs<
  | 'height'
  | 'onAfterPrint'
  | 'onBeforePrint'
  | 'onBeforeUnload'
  | 'onGamepadConnected'
  | 'onGamepadDisconnected'
  | 'onHashChange'
  | 'onLanguageChange'
  | 'onMessage'
  | 'onMessageError'
  | 'onOffline'
  | 'onOnline'
  | 'onPageHide'
  | 'onPageShow'
  | 'onPopState'
  | 'onRejectionHandled'
  | 'onStorage'
  | 'onUnhandledRejection'
  | 'onUnload'
  | 'preserveAspectRatio'
  | 'viewBox'
  | 'width'
  | 'x'
  | 'y'
> &
  SvgAttrs<CustomContext, BaseElement>
