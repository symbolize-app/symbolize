import type * as compute from '@intertwine/lib-compute'

export type EventMap<BaseElement extends Element> = Readonly<
  BaseElement extends HTMLVideoElement ? HTMLVideoElementEventMap
  : BaseElement extends HTMLMediaElement ? HTMLMediaElementEventMap
  : BaseElement extends HTMLBodyElement ? HTMLBodyElementEventMap
  : BaseElement extends HTMLElement ? HTMLElementEventMap
  : BaseElement extends SVGSVGElement ? SVGSVGElementEventMap
  : BaseElement extends SVGElement ? SVGElementEventMap
  : BaseElement extends MathMLElement ? MathMLElementEventMap
  : never
>

const customGlobalEventHandlersEventMap = {
  onAbort: 'abort',
  onAnimationCancel: 'animationcancel',
  onAnimationEnd: 'animationend',
  onAnimationIteration: 'animationiteration',
  onAnimationStart: 'animationstart',
  onAuxClick: 'auxclick',
  onBeforeInput: 'beforeinput',
  onBlur: 'blur',
  onCanPlay: 'canplay',
  onCanPlayThrough: 'canplaythrough',
  onCancel: 'cancel',
  onChange: 'change',
  onClick: 'click',
  onClose: 'close',
  onCompositionEnd: 'compositionend',
  onCompositionStart: 'compositionstart',
  onCompositionUpdate: 'compositionupdate',
  onContextMenu: 'contextmenu',
  onCopy: 'copy',
  onCueChange: 'cuechange',
  onCut: 'cut',
  onDblClick: 'dblclick',
  onDrag: 'drag',
  onDragEnd: 'dragend',
  onDragEnter: 'dragenter',
  onDragLeave: 'dragleave',
  onDragOver: 'dragover',
  onDragStart: 'dragstart',
  onDrop: 'drop',
  onDurationChange: 'durationchange',
  onEmptied: 'emptied',
  onEnded: 'ended',
  onError: 'error',
  onFocus: 'focus',
  onFocusIn: 'focusin',
  onFocusOut: 'focusout',
  onFormData: 'formdata',
  onGotPointerCapture: 'gotpointercapture',
  onInput: 'input',
  onInvalid: 'invalid',
  onKeyDown: 'keydown',
  onKeyPress: 'keypress',
  onKeyUp: 'keyup',
  onLoad: 'load',
  onLoadStart: 'loadstart',
  onLoadedData: 'loadeddata',
  onLoadedMetadata: 'loadedmetadata',
  onLostPointercapture: 'lostpointercapture',
  onMouseDown: 'mousedown',
  onMouseEnter: 'mouseenter',
  onMouseLeave: 'mouseleave',
  onMouseMove: 'mousemove',
  onMouseOut: 'mouseout',
  onMouseOver: 'mouseover',
  onMouseUp: 'mouseup',
  onPaste: 'paste',
  onPause: 'pause',
  onPlay: 'play',
  onPlaying: 'playing',
  onPointerCancel: 'pointercancel',
  onPointerDown: 'pointerdown',
  onPointerEnter: 'pointerenter',
  onPointerLeave: 'pointerleave',
  onPointerMove: 'pointermove',
  onPointerOut: 'pointerout',
  onPointerOver: 'pointerover',
  onPointerUp: 'pointerup',
  onProgress: 'progress',
  onRateChange: 'ratechange',
  onReset: 'reset',
  onResize: 'resize',
  onScroll: 'scroll',
  onScrollEnd: 'scrollend',
  onSecurityPolicyViolation: 'securitypolicyviolation',
  onSeeked: 'seeked',
  onSeeking: 'seeking',
  onSelect: 'select',
  onSelectStart: 'selectstart',
  onSelectionChange: 'selectionchange',
  onSlotChange: 'slotchange',
  onStalled: 'stalled',
  onSubmit: 'submit',
  onSuspend: 'suspend',
  onTimeUpdate: 'timeupdate',
  onToggle: 'toggle',
  onTouchCancel: 'touchcancel',
  onTouchEnd: 'touchend',
  onTouchMove: 'touchmove',
  onTouchStart: 'touchstart',
  onTransitionCancel: 'transitioncancel',
  onTransitionEnd: 'transitionend',
  onTransitionRun: 'transitionrun',
  onTransitionStart: 'transitionstart',
  onVolumeChange: 'volumechange',
  onWaiting: 'waiting',
  onWebkitAnimationEnd: 'webkitanimationend',
  onWebkitAnimationIteration: 'webkitanimationiteration',
  onWebkitAnimationStart: 'webkitanimationstart',
  onWebkitTransitionEnd: 'webkittransitionend',
  onWheel: 'wheel',
} as const

const customElementEventMap = {
  onFullscreenChange: 'fullscreenchange',
  onFullscreenError: 'fullscreenerror',
} as const

const customWindowEventHandlersEventMap = {
  onAfterPrint: 'afterprint',
  onBeforePrint: 'beforeprint',
  onBeforeUnload: 'beforeunload',
  onGamepadConnected: 'gamepadconnected',
  onGamepadDisconnected: 'gamepaddisconnected',
  onHashChange: 'hashchange',
  onLanguageChange: 'languagechange',
  onMessage: 'message',
  onMessageError: 'messageerror',
  onOffline: 'offline',
  onOnline: 'online',
  onPageHide: 'pagehide',
  onPageShow: 'pageshow',
  onPopState: 'popstate',
  onRejectionHandled: 'rejectionhandled',
  onStorage: 'storage',
  onUnhandledRejection: 'unhandledrejection',
  onUnload: 'unload',
} as const

// eslint-disable-next-line @typescript-eslint/naming-convention -- match original
const customHTMLMediaElementEventMap = {
  onEncrypted: 'encrypted',
  onWaitingForKey: 'waitingforkey',
} as const

// eslint-disable-next-line @typescript-eslint/naming-convention -- match original
const customHTMLVideoElementEventMap = {
  onEnterPictureInPicture: 'enterpictureinpicture',
  onLeavePictureInPicture: 'leavepictureinpicture',
} as const

export type CustomEventMap = typeof customEventMap

export const customEventMap = {
  ...customGlobalEventHandlersEventMap,
  ...customElementEventMap,
  ...customWindowEventHandlersEventMap,
  ...customHTMLMediaElementEventMap,
  ...customHTMLVideoElementEventMap,
} as const

export interface SkippedMap {
  readonly accessKeyLabel: never
  readonly align: never
  readonly attributeStyleMap: never
  readonly ['baseURI']: never
  readonly childElementCount: never
  readonly classList: never
  readonly className: never
  readonly clientHeight: never
  readonly clientLeft: never
  readonly clientTop: never
  readonly clientWidth: never
  readonly currentScale: never
  readonly currentTranslate: never
  readonly dataset: never
  readonly ['innerHTML']: never
  readonly innerText: never
  readonly isConnected: never
  readonly isContentEditable: never
  readonly localName: never
  readonly ['namespaceURI']: never
  readonly nodeName: never
  readonly nodeType: never
  readonly nodeValue: never
  readonly offsetHeight: never
  readonly offsetLeft: never
  readonly offsetTop: never
  readonly offsetWidth: never
  readonly ['outerHTML']: never
  readonly outerText: never
  readonly part: never
  readonly popover: never
  readonly popoverTargetAction: never
  readonly prefix: never
  readonly scrollHeight: never
  readonly scrollLeft: never
  readonly scrollTop: never
  readonly scrollWidth: never
  readonly style: never
  readonly tagName: never
  readonly textContent: never
  readonly transform: never
  readonly validationMessage: never
  readonly validity: never
  readonly willValidate: never
}

export interface OverrideMap {
  readonly ariaAtomic: compute.ComputationOpt<'false' | 'true' | null>
  readonly ariaAutoComplete: compute.ComputationOpt<
    'both' | 'inline' | 'list' | 'none' | null
  >
  readonly ariaBusy: compute.ComputationOpt<'false' | 'true' | null>
  readonly ariaChecked: compute.ComputationOpt<
    'false' | 'mixed' | 'true' | 'undefined' | null
  >
  readonly ariaCurrent: compute.ComputationOpt<
    | 'date'
    | 'false'
    | 'location'
    | 'page'
    | 'step'
    | 'time'
    | 'true'
    | null
  >
  readonly ariaDisabled: compute.ComputationOpt<'false' | 'true' | null>
  readonly ariaExpanded: compute.ComputationOpt<
    'false' | 'true' | 'undefined' | null
  >
  readonly ariaHasPopup: compute.ComputationOpt<
    | 'dialog'
    | 'false'
    | 'grid'
    | 'listbox'
    | 'menu'
    | 'tree'
    | 'true'
    | null
  >
  readonly ariaHidden: compute.ComputationOpt<
    'false' | 'true' | 'undefined' | null
  >
  readonly ariaLive: compute.ComputationOpt<
    'assertive' | 'off' | 'polite' | null
  >
  readonly ariaModal: compute.ComputationOpt<'false' | 'true' | null>
  readonly ariaMultiLine: compute.ComputationOpt<'false' | 'true' | null>
  readonly ariaMultiSelectable: compute.ComputationOpt<
    'false' | 'true' | null
  >
  readonly ariaOrientation: compute.ComputationOpt<
    'horizontal' | 'undefined' | 'vertical' | null
  >
  readonly ariaPressed: compute.ComputationOpt<
    'false' | 'mixed' | 'true' | 'undefined' | null
  >
  readonly ariaReadOnly: compute.ComputationOpt<'false' | 'true' | null>
  readonly ariaRequired: compute.ComputationOpt<'false' | 'true' | null>
  readonly ariaSelected: compute.ComputationOpt<
    'false' | 'true' | 'undefined' | null
  >
  readonly ariaSort: compute.ComputationOpt<
    'ascending' | 'descending' | 'none' | 'other' | null
  >
  readonly autocapitalize: compute.ComputationOpt<
    'characters' | 'none' | 'sentences' | 'words'
  >
  readonly dir: compute.ComputationOpt<'auto' | 'ltr' | 'rtl'>
  readonly enterKeyHint: compute.ComputationOpt<
    'done' | 'enter' | 'go' | 'next' | 'previous' | 'search' | 'send'
  >
  readonly formEnctype: compute.ComputationOpt<
    | 'application/x-www-form-urlencoded'
    | 'multipart/form-data'
    | 'text/plain'
  >
  readonly formMethod: compute.ComputationOpt<'dialog' | 'get' | 'post'>
  readonly hidden: compute.ComputationOpt<boolean | 'until-found'>
  readonly inputMode: compute.ComputationOpt<
    | 'decimal'
    | 'email'
    | 'none'
    | 'numeric'
    | 'search'
    | 'tel'
    | 'text'
    | 'url'
  >
  readonly nonce: compute.ComputationOpt<string>
}
