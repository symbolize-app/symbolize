import type * as conveyContext from '@/context.ts'
import type * as conveyData from '@/data.ts'
import type * as conveyFragment from '@/fragment.ts'
import type * as compute from '@intertwine/lib-compute'
import type * as contrast from '@intertwine/lib-contrast'

export type Listener<SpecificEvent> = (
  event: SpecificEvent,
) => Promise<void> | void

export type Attrs<CustomContext, BaseElement extends Element> = PickAttrs<
  | 'ariaAtomic'
  | 'ariaAutoComplete'
  | 'ariaBusy'
  | 'ariaChecked'
  | 'ariaColCount'
  | 'ariaColIndex'
  | 'ariaColSpan'
  | 'ariaCurrent'
  | 'ariaDescription'
  | 'ariaDisabled'
  | 'ariaExpanded'
  | 'ariaHasPopup'
  | 'ariaHidden'
  | 'ariaInvalid'
  | 'ariaKeyShortcuts'
  | 'ariaLabel'
  | 'ariaLevel'
  | 'ariaLive'
  | 'ariaModal'
  | 'ariaMultiLine'
  | 'ariaMultiSelectable'
  | 'ariaOrientation'
  | 'ariaPlaceholder'
  | 'ariaPosInSet'
  | 'ariaPressed'
  | 'ariaReadOnly'
  | 'ariaRequired'
  | 'ariaRoleDescription'
  | 'ariaRowCount'
  | 'ariaRowIndex'
  | 'ariaRowSpan'
  | 'ariaSelected'
  | 'ariaSetSize'
  | 'ariaSort'
  | 'ariaValueMax'
  | 'ariaValueMin'
  | 'ariaValueNow'
  | 'ariaValueText'
  | 'autofocus'
  | 'className'
  | 'id'
  | 'nonce'
  | 'onAbort'
  | 'onAnimationCancel'
  | 'onAnimationEnd'
  | 'onAnimationIteration'
  | 'onAnimationStart'
  | 'onAuxClick'
  | 'onBeforeInput'
  | 'onBlur'
  | 'onCancel'
  | 'onCanPlay'
  | 'onCanPlayThrough'
  | 'onChange'
  | 'onClick'
  | 'onClose'
  | 'onCompositionEnd'
  | 'onCompositionStart'
  | 'onCompositionUpdate'
  | 'onContextMenu'
  | 'onCopy'
  | 'onCueChange'
  | 'onCut'
  | 'onDblClick'
  | 'onDrag'
  | 'onDragEnd'
  | 'onDragEnter'
  | 'onDragLeave'
  | 'onDragOver'
  | 'onDragStart'
  | 'onDrop'
  | 'onDurationChange'
  | 'onEmptied'
  | 'onEnded'
  | 'onError'
  | 'onFocus'
  | 'onFocusIn'
  | 'onFocusOut'
  | 'onFormData'
  | 'onFullscreenChange'
  | 'onFullscreenError'
  | 'onGotPointerCapture'
  | 'onInput'
  | 'onInvalid'
  | 'onKeyDown'
  | 'onKeyPress'
  | 'onKeyUp'
  | 'onLoad'
  | 'onLoadedData'
  | 'onLoadedMetadata'
  | 'onLoadStart'
  | 'onLostPointercapture'
  | 'onMouseDown'
  | 'onMouseEnter'
  | 'onMouseLeave'
  | 'onMouseMove'
  | 'onMouseOut'
  | 'onMouseOver'
  | 'onMouseUp'
  | 'onPaste'
  | 'onPause'
  | 'onPlay'
  | 'onPlaying'
  | 'onPointerCancel'
  | 'onPointerDown'
  | 'onPointerEnter'
  | 'onPointerLeave'
  | 'onPointerMove'
  | 'onPointerOut'
  | 'onPointerOver'
  | 'onPointerUp'
  | 'onProgress'
  | 'onRateChange'
  | 'onReset'
  | 'onResize'
  | 'onScroll'
  | 'onScrollEnd'
  | 'onSecurityPolicyViolation'
  | 'onSeeked'
  | 'onSeeking'
  | 'onSelect'
  | 'onSelectionChange'
  | 'onSelectStart'
  | 'onSlotChange'
  | 'onStalled'
  | 'onSubmit'
  | 'onSuspend'
  | 'onTimeUpdate'
  | 'onToggle'
  | 'onTouchCancel'
  | 'onTouchEnd'
  | 'onTouchMove'
  | 'onTouchStart'
  | 'onTransitionCancel'
  | 'onTransitionEnd'
  | 'onTransitionRun'
  | 'onTransitionStart'
  | 'onVolumeChange'
  | 'onWaiting'
  | 'onWebkitAnimationEnd'
  | 'onWebkitAnimationIteration'
  | 'onWebkitAnimationStart'
  | 'onWebkitTransitionEnd'
  | 'onWheel'
  | 'role'
  | 'slot'
  | 'tabIndex'
> & {
  readonly content?: conveyFragment.FragmentOpt<CustomContext>
  readonly onAdd?: Listener<OnAddEvent<BaseElement>>
  readonly style?: compute.NodeOpt<contrast.AtomOpt>
}

export interface OnAddEvent<BaseElement extends Element> {
  readonly ctx: conveyContext.ScopedContext
  readonly element: BaseElement
}

export type PickAttrs<Keys extends keyof AllAttrs> = {
  readonly [Key in Keys]?: AllAttrs[Key]['type']
}

export type PickAttrsForType<
  Type extends string,
  Keys extends keyof AllAttrs,
> = {
  readonly [Key in Keys]?: AllAttrs[Key]['type']
} & {
  readonly type: Type
}

export enum ElementAttrKind {
  boolean = 0,
  content = 1,
  listener = 2,
  onAdd = 3,
  string = 4,
  style = 5,
}

interface BooleanAttrDefinition<Type> {
  readonly kind: ElementAttrKind.boolean
  readonly name: string
  readonly type?: compute.NodeOpt<Type>
}

function booleanAttr<Type = boolean>(
  name: string,
): BooleanAttrDefinition<Type> {
  return { kind: ElementAttrKind.boolean, name }
}

interface ContentAttrDefinition {
  readonly kind: ElementAttrKind.content
  readonly type?: never
}

function contentAttr(): ContentAttrDefinition {
  return { kind: ElementAttrKind.content }
}

interface ListenerAttrDefinition<Type> {
  readonly kind: ElementAttrKind.listener
  readonly name: string
  readonly type?: Listener<Type>
}

function listenerAttr<Type>(name: string): ListenerAttrDefinition<Type> {
  return { kind: ElementAttrKind.listener, name }
}

interface OnAddAttrDefinition {
  readonly kind: ElementAttrKind.onAdd
  readonly type?: never
}

function onAddAttr(): OnAddAttrDefinition {
  return { kind: ElementAttrKind.onAdd }
}

interface StringAttrDefinition<Type> {
  readonly kind: ElementAttrKind.string
  readonly name: string
  readonly type?: compute.NodeOpt<Type | null>
}

function stringAttr<Type = string>(
  name: string,
): StringAttrDefinition<Type> {
  return { kind: ElementAttrKind.string, name }
}

interface StyleAttrDefinition {
  readonly kind: ElementAttrKind.style
  readonly type?: never
}

function styleAttr(): StyleAttrDefinition {
  return { kind: ElementAttrKind.style }
}

export type AllAttrs = Readonly<typeof allAttrs>

// Redeclare these types locally, to avoid triggering https://github.com/microsoft/TypeScript/issues/47663
type LengthUnit = contrast.LengthUnit
type Length<Unit extends LengthUnit = LengthUnit> = contrast.Length<Unit>
type Pct = contrast.Pct

export const allAttrs = {
  accessKey: stringAttr('accesskey'),
  altText: stringAttr('alttext'),
  ariaAtomic: stringAttr<boolean>('aria-atomic'),
  ariaAutoComplete: stringAttr<'both' | 'inline' | 'list' | 'none'>(
    'aria-autocomplete',
  ),
  ariaBusy: stringAttr<boolean>('aria-busy'),
  ariaChecked: stringAttr<boolean | 'mixed' | 'undefined'>('aria-checked'),
  ariaColCount: stringAttr('aria-colcount'),
  ariaColIndex: stringAttr('aria-colindex'),
  ariaColSpan: stringAttr('aria-colspan'),
  ariaCurrent: stringAttr<
    boolean | 'date' | 'location' | 'page' | 'step' | 'time'
  >('aria-current'),
  ariaDescription: stringAttr('aria-description'),
  ariaDisabled: stringAttr<boolean>('aria-disabled'),
  ariaExpanded: stringAttr<boolean | 'undefined'>('aria-expanded'),
  ariaHasPopup: stringAttr<
    boolean | 'dialog' | 'grid' | 'listbox' | 'menu' | 'tree'
  >('aria-haspopup'),
  ariaHidden: stringAttr<boolean | 'undefined'>('aria-hidden'),
  ariaInvalid: stringAttr('aria-invalid'),
  ariaKeyShortcuts: stringAttr('aria-keyshortcuts'),
  ariaLabel: stringAttr('aria-label'),
  ariaLevel: stringAttr('aria-level'),
  ariaLive: stringAttr<'assertive' | 'off' | 'polite'>('aria-live'),
  ariaModal: stringAttr<boolean>('aria-modal'),
  ariaMultiLine: stringAttr<boolean>('aria-multiline'),
  ariaMultiSelectable: stringAttr<boolean>('aria-multiselectable'),
  ariaOrientation: stringAttr<'horizontal' | 'undefined' | 'vertical'>(
    'aria-orientation',
  ),
  ariaPlaceholder: stringAttr('aria-placeholder'),
  ariaPosInSet: stringAttr('aria-posinset'),
  ariaPressed: stringAttr<boolean | 'mixed' | 'undefined'>('aria-pressed'),
  ariaReadOnly: stringAttr<boolean>('aria-readonly'),
  ariaRequired: stringAttr<boolean>('aria-required'),
  ariaRoleDescription: stringAttr('aria-roledescription'),
  ariaRowCount: stringAttr('aria-rowcount'),
  ariaRowIndex: stringAttr('aria-rowindex'),
  ariaRowSpan: stringAttr('aria-rowspan'),
  ariaSelected: stringAttr<boolean | 'undefined'>('aria-selected'),
  ariaSetSize: stringAttr('aria-setsize'),
  ariaSort: stringAttr<'ascending' | 'descending' | 'none' | 'other'>(
    'aria-sort',
  ),
  ariaValueMax: stringAttr('aria-valuemax'),
  ariaValueMin: stringAttr('aria-valuemin'),
  ariaValueNow: stringAttr('aria-valuenow'),
  ariaValueText: stringAttr('aria-valuetext'),
  autocapitalize: stringAttr<
    'characters' | 'none' | 'sentences' | 'words'
  >('autocapitalize'),
  autocomplete: stringAttr<
    readonly ('email' | 'name' | `section-${string}`)[] | 'off' | 'on'
  >('autocomplete'),
  autofocus: booleanAttr('autofocus'),
  checked: booleanAttr('checked'),
  className: stringAttr<readonly string[]>('class'),
  content: contentAttr(),
  contentEditable: stringAttr<boolean | 'plaintext-only'>(
    'contenteditable',
  ),
  dir: stringAttr<'auto' | 'ltr' | 'rtl'>('dir'),
  disabled: booleanAttr('disabled'),
  display: stringAttr<'block' | 'inline'>('display'),
  displayStyle: stringAttr<boolean>('displaystyle'),
  draggable: stringAttr<boolean>('draggable'),
  enterKeyHint: stringAttr<
    'done' | 'enter' | 'go' | 'next' | 'previous' | 'search' | 'send'
  >('enterkeyhint'),
  fence: stringAttr<boolean>('fence'),
  form: stringAttr('form'),
  formAction: stringAttr('formaction'),
  formEnctype: stringAttr<
    | 'application/x-www-form-urlencoded'
    | 'multipart/form-data'
    | 'text/plain'
  >('formenctype'),
  formMethod: stringAttr<'dialog' | 'get' | 'post'>('formmethod'),
  formNoValidate: booleanAttr('formnovalidate'),
  formTarget: stringAttr('formtarget'),
  height: stringAttr<conveyData.SvgLengthPctOpt>('height'),
  hidden: booleanAttr<boolean | 'until-found'>('hidden'),
  id: stringAttr('id'),
  inert: booleanAttr('inert'),
  inputMode: stringAttr<
    | 'decimal'
    | 'email'
    | 'none'
    | 'numeric'
    | 'search'
    | 'tel'
    | 'text'
    | 'url'
  >('inputmode'),
  lSpace: stringAttr<Length | Pct>('lspace'),
  lang: stringAttr('lang'),
  largeOp: stringAttr<boolean>('largeop'),
  list: stringAttr('list'),
  mathDir: stringAttr<'ltr' | 'rtl'>('dir'),
  mathVariant: stringAttr<'normal'>('mathvariant'),
  maxLength: stringAttr<number>('maxlength'),
  maxSize: stringAttr<Length | Pct>('maxsize'),
  minLength: stringAttr<number>('minlength'),
  minSize: stringAttr<Length | Pct>('minsize'),
  moveableLimits: stringAttr<boolean>('moveablelimits'),
  name: stringAttr('name'),
  nonce: stringAttr('nonce'),
  onAbort: listenerAttr<UIEvent>('abort'),
  onAdd: onAddAttr(),
  onAfterPrint: listenerAttr<Event>('afterprint'),
  onAnimationCancel: listenerAttr<AnimationEvent>('animationcancel'),
  onAnimationEnd: listenerAttr<AnimationEvent>('animationend'),
  onAnimationIteration: listenerAttr<AnimationEvent>('animationiteration'),
  onAnimationStart: listenerAttr<AnimationEvent>('animationstart'),
  onAuxClick: listenerAttr<MouseEvent>('auxclick'),
  onBeforeInput: listenerAttr<InputEvent>('beforeinput'),
  onBeforePrint: listenerAttr<Event>('beforeprint'),
  onBeforeUnload: listenerAttr<Event>('beforeunload'),
  onBlur: listenerAttr<FocusEvent>('blur'),
  onCanPlay: listenerAttr<Event>('canplay'),
  onCanPlayThrough: listenerAttr<Event>('canplaythrough'),
  onCancel: listenerAttr<Event>('cancel'),
  onChange: listenerAttr<Event>('change'),
  onClick: listenerAttr<MouseEvent>('click'),
  onClose: listenerAttr<Event>('close'),
  onCompositionEnd: listenerAttr<CompositionEvent>('compositionend'),
  onCompositionStart: listenerAttr<CompositionEvent>('compositionstart'),
  onCompositionUpdate: listenerAttr<CompositionEvent>('compositionupdate'),
  onContextMenu: listenerAttr<MouseEvent>('contextmenu'),
  onCopy: listenerAttr<ClipboardEvent>('copy'),
  onCueChange: listenerAttr<Event>('cuechange'),
  onCut: listenerAttr<ClipboardEvent>('cut'),
  onDblClick: listenerAttr<MouseEvent>('dblclick'),
  onDrag: listenerAttr<DragEvent>('drag'),
  onDragEnd: listenerAttr<DragEvent>('dragend'),
  onDragEnter: listenerAttr<DragEvent>('dragenter'),
  onDragLeave: listenerAttr<DragEvent>('dragleave'),
  onDragOver: listenerAttr<DragEvent>('dragover'),
  onDragStart: listenerAttr<DragEvent>('dragstart'),
  onDrop: listenerAttr<DragEvent>('drop'),
  onDurationChange: listenerAttr<Event>('durationchange'),
  onEmptied: listenerAttr<Event>('emptied'),
  onEnded: listenerAttr<Event>('ended'),
  onError: listenerAttr<ErrorEvent>('error'),
  onFocus: listenerAttr<FocusEvent>('focus'),
  onFocusIn: listenerAttr<FocusEvent>('focusin'),
  onFocusOut: listenerAttr<FocusEvent>('focusout'),
  onFormData: listenerAttr<FormDataEvent>('formdata'),
  onFullscreenChange: listenerAttr<Event>('fullscreenchange'),
  onFullscreenError: listenerAttr<Event>('fullscreenerror'),
  onGamepadConnected: listenerAttr<GamepadEvent>('gamepadconnected'),
  onGamepadDisconnected: listenerAttr<GamepadEvent>('gamepaddisconnected'),
  onGotPointerCapture: listenerAttr<PointerEvent>('gotpointercapture'),
  onHashChange: listenerAttr<HashChangeEvent>('hashchange'),
  onInput: listenerAttr<Event>('input'),
  onInvalid: listenerAttr<Event>('invalid'),
  onKeyDown: listenerAttr<KeyboardEvent>('keydown'),
  onKeyPress: listenerAttr<KeyboardEvent>('keypress'),
  onKeyUp: listenerAttr<KeyboardEvent>('keyup'),
  onLanguageChange: listenerAttr<Event>('languagechange'),
  onLoad: listenerAttr<Event>('load'),
  onLoadStart: listenerAttr<Event>('loadstart'),
  onLoadedData: listenerAttr<Event>('loadeddata'),
  onLoadedMetadata: listenerAttr<Event>('loadedmetadata'),
  onLostPointercapture: listenerAttr<PointerEvent>('lostpointercapture'),
  onMessage: listenerAttr<MessageEvent>('message'),
  onMessageError: listenerAttr<MessageEvent>('messageerror'),
  onMouseDown: listenerAttr<MouseEvent>('mousedown'),
  onMouseEnter: listenerAttr<MouseEvent>('mouseenter'),
  onMouseLeave: listenerAttr<MouseEvent>('mouseleave'),
  onMouseMove: listenerAttr<MouseEvent>('mousemove'),
  onMouseOut: listenerAttr<MouseEvent>('mouseout'),
  onMouseOver: listenerAttr<MouseEvent>('mouseover'),
  onMouseUp: listenerAttr<MouseEvent>('mouseup'),
  onOffline: listenerAttr<Event>('offline'),
  onOnline: listenerAttr<Event>('online'),
  onPageHide: listenerAttr<PageTransitionEvent>('pagehide'),
  onPageShow: listenerAttr<PageTransitionEvent>('pageshow'),
  onPaste: listenerAttr<ClipboardEvent>('paste'),
  onPause: listenerAttr<Event>('pause'),
  onPlay: listenerAttr<Event>('play'),
  onPlaying: listenerAttr<Event>('playing'),
  onPointerCancel: listenerAttr<PointerEvent>('pointercancel'),
  onPointerDown: listenerAttr<PointerEvent>('pointerdown'),
  onPointerEnter: listenerAttr<PointerEvent>('pointerenter'),
  onPointerLeave: listenerAttr<PointerEvent>('pointerleave'),
  onPointerMove: listenerAttr<PointerEvent>('pointermove'),
  onPointerOut: listenerAttr<PointerEvent>('pointerout'),
  onPointerOver: listenerAttr<PointerEvent>('pointerover'),
  onPointerUp: listenerAttr<PointerEvent>('pointerup'),
  onPopState: listenerAttr<PopStateEvent>('popstate'),
  onProgress: listenerAttr<ProgressEvent>('progress'),
  onRateChange: listenerAttr<Event>('ratechange'),
  onRejectionHandled:
    listenerAttr<PromiseRejectionEvent>('rejectionhandled'),
  onReset: listenerAttr<Event>('reset'),
  onResize: listenerAttr<UIEvent>('resize'),
  onScroll: listenerAttr<Event>('scroll'),
  onScrollEnd: listenerAttr<Event>('scrollend'),
  onSecurityPolicyViolation: listenerAttr<SecurityPolicyViolationEvent>(
    'securitypolicyviolation',
  ),
  onSeeked: listenerAttr<Event>('seeked'),
  onSeeking: listenerAttr<Event>('seeking'),
  onSelect: listenerAttr<Event>('select'),
  onSelectStart: listenerAttr<Event>('selectstart'),
  onSelectionChange: listenerAttr<Event>('selectionchange'),
  onSlotChange: listenerAttr<Event>('slotchange'),
  onStalled: listenerAttr<Event>('stalled'),
  onStorage: listenerAttr<StorageEvent>('storage'),
  onSubmit: listenerAttr<SubmitEvent>('submit'),
  onSuspend: listenerAttr<Event>('suspend'),
  onTimeUpdate: listenerAttr<Event>('timeupdate'),
  onToggle: listenerAttr<Event>('toggle'),
  onTouchCancel: listenerAttr<TouchEvent>('touchcancel'),
  onTouchEnd: listenerAttr<TouchEvent>('touchend'),
  onTouchMove: listenerAttr<TouchEvent>('touchmove'),
  onTouchStart: listenerAttr<TouchEvent>('touchstart'),
  onTransitionCancel: listenerAttr<TransitionEvent>('transitioncancel'),
  onTransitionEnd: listenerAttr<TransitionEvent>('transitionend'),
  onTransitionRun: listenerAttr<TransitionEvent>('transitionrun'),
  onTransitionStart: listenerAttr<TransitionEvent>('transitionstart'),
  onUnhandledRejection: listenerAttr<PromiseRejectionEvent>(
    'unhandledrejection',
  ),
  onUnload: listenerAttr<Event>('unload'),
  onVolumeChange: listenerAttr<Event>('volumechange'),
  onWaiting: listenerAttr<Event>('waiting'),
  onWebkitAnimationEnd: listenerAttr<Event>('webkitanimationend'),
  onWebkitAnimationIteration: listenerAttr<Event>(
    'webkitanimationiteration',
  ),
  onWebkitAnimationStart: listenerAttr<Event>('webkitanimationstart'),
  onWebkitTransitionEnd: listenerAttr<Event>('webkittransitionend'),
  onWheel: listenerAttr<WheelEvent>('wheel'),
  operatorForm: stringAttr<'infix' | 'postfix' | 'prefix'>('form'),
  pathLength: stringAttr<number>('pathLength'),
  pattern: stringAttr('pattern'),
  placeholder: stringAttr('placeholder'),
  preserveAspectRatio: stringAttr<conveyData.SvgPreserveAspectRatioOpt>(
    'preserveAspectRatio',
  ),
  rSpace: stringAttr<Length | Pct>('rspace'),
  readOnly: booleanAttr('readonly'),
  required: booleanAttr('required'),
  requiredExtensions: stringAttr<readonly string[]>('requiredExtensions'),
  role: stringAttr('role'),
  rx: stringAttr<conveyData.SvgLengthPctOpt>('rx'),
  ry: stringAttr<conveyData.SvgLengthPctOpt>('ry'),
  scriptLevel: stringAttr<number>('scriptlevel'),
  separator: stringAttr<boolean>('separator'),
  size: stringAttr<number>('size'),
  slot: stringAttr('slot'),
  spellcheck: stringAttr('spellcheck'),
  stretchy: stringAttr<boolean>('stretchy'),
  style: styleAttr(),
  symmetric: stringAttr<boolean>('symmetric'),
  systemLanguage: stringAttr<readonly string[]>('systemLanguage'),
  tabIndex: stringAttr<number>('tabindex'),
  title: stringAttr('title'),
  translate: stringAttr<'no' | 'yes'>('translate'),
  type: stringAttr('type'),
  value: stringAttr('value'),
  viewBox: stringAttr<conveyData.Rect>('viewBox'),
  width: stringAttr<conveyData.SvgLengthPctOpt>('width'),
  x: stringAttr<conveyData.SvgLengthPctOpt>('x'),
  y: stringAttr<conveyData.SvgLengthPctOpt>('y'),
}
