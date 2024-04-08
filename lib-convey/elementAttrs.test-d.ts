import type * as conveyElementAttrs from '@/elementAttrs.ts'

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

export type OverrideMap = Required<
  conveyElementAttrs.PickAttrs<
    | 'ariaAtomic'
    | 'ariaAutoComplete'
    | 'ariaBusy'
    | 'ariaChecked'
    | 'ariaCurrent'
    | 'ariaDisabled'
    | 'ariaExpanded'
    | 'ariaHasPopup'
    | 'ariaHidden'
    | 'ariaLive'
    | 'ariaModal'
    | 'ariaMultiLine'
    | 'ariaMultiSelectable'
    | 'ariaOrientation'
    | 'ariaPressed'
    | 'ariaReadOnly'
    | 'ariaRequired'
    | 'ariaSelected'
    | 'ariaSort'
    | 'autocapitalize'
    | 'contentEditable'
    | 'dir'
    | 'draggable'
    | 'enterKeyHint'
    | 'formEnctype'
    | 'formMethod'
    | 'hidden'
    | 'inputMode'
    | 'nonce'
    | 'spellcheck'
    | 'translate'
  >
>
