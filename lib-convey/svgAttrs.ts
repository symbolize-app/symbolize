import type * as conveyElementAttrs from '@/elementAttrs.ts'

export interface AttrsTagNameMap<CustomContext> {
  readonly g: Attrs<CustomContext, SVGGElement>
  readonly rect: RectAttrs<CustomContext, SVGRectElement>
  readonly svg: SvgAttrs<CustomContext, SVGSVGElement>
}

export type Attrs<
  CustomContext,
  BaseElement extends Element,
> = conveyElementAttrs.Attrs<CustomContext, BaseElement> &
  conveyElementAttrs.PickAttrs<'requiredExtensions' | 'systemLanguage'>

export type RectAttrs<CustomContext, BaseElement extends Element> = Attrs<
  CustomContext,
  BaseElement
> &
  conveyElementAttrs.PickAttrs<
    'height' | 'pathLength' | 'rx' | 'ry' | 'width' | 'x' | 'y'
  >

export type SvgAttrs<CustomContext, BaseElement extends Element> = Attrs<
  CustomContext,
  BaseElement
> &
  conveyElementAttrs.PickAttrs<
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
  >
