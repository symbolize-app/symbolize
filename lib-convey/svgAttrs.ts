import type * as conveyElementAttrs from '@/elementAttrs.ts'

export interface SvgAttrsTagNameMap<CustomContext> {
  readonly g: SvgAttrs<CustomContext, SVGGElement>
  readonly rect: RectAttrs<CustomContext, SVGRectElement>
  readonly svg: SvgSvgAttrs<CustomContext, SVGSVGElement>
}

export type SvgAttrsByElement<
  CustomElement extends SVGElement,
  CustomContext,
> = {
  [Key in keyof SvgAttrsTagNameMap<unknown>]: SvgAttrs<
    CustomContext,
    CustomElement
  > extends SvgAttrsTagNameMap<CustomContext>[Key] ?
    SvgAttrsTagNameMap<CustomContext>[Key]
  : never
}[keyof SvgAttrsTagNameMap<unknown>]

export type SvgAttrs<
  CustomContext,
  BaseElement extends Element,
> = conveyElementAttrs.Attrs<CustomContext, BaseElement> &
  conveyElementAttrs.PickAttrs<'requiredExtensions' | 'systemLanguage'>

export type RectAttrs<
  CustomContext,
  BaseElement extends Element,
> = conveyElementAttrs.PickAttrs<
  'height' | 'pathLength' | 'rx' | 'ry' | 'width' | 'x' | 'y'
> &
  SvgAttrs<CustomContext, BaseElement>

export type SvgSvgAttrs<
  CustomContext,
  BaseElement extends Element,
> = conveyElementAttrs.PickAttrs<
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
