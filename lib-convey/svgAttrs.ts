import type * as conveyElementAttrs from '@/elementAttrs.ts'

export interface AttrsTagNameMap<CustomContext> {
  readonly g: Attrs<CustomContext>
  readonly rect: RectAttrs<CustomContext>
  readonly svg: SvgAttrs<CustomContext>
}

export type Attrs<CustomContext> =
  conveyElementAttrs.Attrs<CustomContext> &
    conveyElementAttrs.PickAttrs<'requiredExtensions' | 'systemLanguage'>

export type RectAttrs<CustomContext> = Attrs<CustomContext> &
  conveyElementAttrs.PickAttrs<
    'height' | 'pathLength' | 'rx' | 'ry' | 'width' | 'x' | 'y'
  >

export type SvgAttrs<CustomContext> = Attrs<CustomContext> &
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
