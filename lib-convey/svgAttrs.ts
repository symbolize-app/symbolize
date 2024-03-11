import type * as conveyData from '@/data.ts'
import type * as conveyElementAttrs from '@/elementAttrs.ts'
import type * as compute from '@intertwine/lib-compute'

export interface AttrsTagNameMap<CustomContext> {
  readonly g: Attrs<CustomContext>
  readonly rect: RectAttrs<CustomContext>
  readonly svg: SvgAttrs<CustomContext>
}

export interface Attrs<CustomContext>
  extends conveyElementAttrs.Attrs<CustomContext> {
  readonly requiredExtensions?: compute.ComputationOpt<string[]>
  readonly systemLanguage?: compute.ComputationOpt<string[]>
}

export interface RectAttrs<CustomContext> extends Attrs<CustomContext> {
  readonly height?: compute.ComputationOpt<conveyData.SvgLengthOpt>
  readonly pathLength?: compute.ComputationOpt<number>
  readonly rx?: compute.ComputationOpt<conveyData.SvgLengthOpt>
  readonly ry?: compute.ComputationOpt<conveyData.SvgLengthOpt>
  readonly width?: compute.ComputationOpt<conveyData.SvgLengthOpt>
  readonly x?: compute.ComputationOpt<conveyData.SvgLengthOpt>
  readonly y?: compute.ComputationOpt<conveyData.SvgLengthOpt>
}

export interface SvgAttrs<CustomContext> extends Attrs<CustomContext> {
  readonly height?: compute.ComputationOpt<conveyData.SvgLengthOpt>
  readonly onAfterPrint?: conveyElementAttrs.Listener<Event>
  readonly onBeforePrint?: conveyElementAttrs.Listener<Event>
  readonly onBeforeUnload?: conveyElementAttrs.Listener<Event>
  readonly onGamepadConnected?: conveyElementAttrs.Listener<GamepadEvent>
  readonly onGamepadDisconnected?: conveyElementAttrs.Listener<GamepadEvent>
  readonly onHashChange?: conveyElementAttrs.Listener<HashChangeEvent>
  readonly onLanguageChange?: conveyElementAttrs.Listener<Event>
  readonly onMessage?: conveyElementAttrs.Listener<MessageEvent>
  readonly onMessageError?: conveyElementAttrs.Listener<MessageEvent>
  readonly onOffline?: conveyElementAttrs.Listener<Event>
  readonly onOnline?: conveyElementAttrs.Listener<Event>
  readonly onPageHide?: conveyElementAttrs.Listener<PageTransitionEvent>
  readonly onPageShow?: conveyElementAttrs.Listener<PageTransitionEvent>
  readonly onPopState?: conveyElementAttrs.Listener<PopStateEvent>
  readonly onRejectionHandled?: conveyElementAttrs.Listener<PromiseRejectionEvent>
  readonly onStorage?: conveyElementAttrs.Listener<StorageEvent>
  readonly onUnhandledRejection?: conveyElementAttrs.Listener<PromiseRejectionEvent>
  readonly onUnload?: conveyElementAttrs.Listener<Event>
  readonly preserveAspectRatio?: compute.ComputationOpt<conveyData.SvgPreserveAspectRatioOpt>
  readonly viewBox?: compute.ComputationOpt<conveyData.Rect>
  readonly width?: compute.ComputationOpt<conveyData.SvgLengthOpt>
  readonly x?: compute.ComputationOpt<conveyData.SvgLengthOpt>
  readonly y?: compute.ComputationOpt<conveyData.SvgLengthOpt>
}
