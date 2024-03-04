export type CustomEventMap = typeof customEventMap

export type EventMap<BaseElement extends Element> = Readonly<
  BaseElement extends HTMLVideoElement ? HTMLVideoElementEventMap
  : BaseElement extends HTMLMediaElement ? HTMLMediaElementEventMap
  : BaseElement extends HTMLBodyElement ? HTMLBodyElementEventMap
  : BaseElement extends HTMLElement ? HTMLElementEventMap
  : BaseElement extends MathMLElement ? MathMLElementEventMap
  : BaseElement extends SVGSVGElement ? SVGSVGElementEventMap
  : BaseElement extends SVGElement ? SVGElementEventMap
  : ElementEventMap
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

export const customEventMap = {
  ...customGlobalEventHandlersEventMap,
  ...customElementEventMap,
  ...customWindowEventHandlersEventMap,
  ...customHTMLMediaElementEventMap,
  ...customHTMLVideoElementEventMap,
}
