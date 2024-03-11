import type * as conveyFragment from '@/fragment.ts'
import type * as compute from '@intertwine/lib-compute'

export type Listener<SpecificEvent> = (
  event: SpecificEvent,
) => Promise<void> | void

export interface Attrs<CustomContext> {
  readonly ariaAtomic?: compute.ComputationOpt<'false' | 'true' | null>
  readonly ariaAutoComplete?: compute.ComputationOpt<
    'both' | 'inline' | 'list' | 'none' | null
  >
  readonly ariaBusy?: compute.ComputationOpt<'false' | 'true' | null>
  readonly ariaChecked?: compute.ComputationOpt<
    'false' | 'mixed' | 'true' | 'undefined' | null
  >
  readonly ariaColCount?: compute.ComputationOpt<string | null>
  readonly ariaColIndex?: compute.ComputationOpt<string | null>
  readonly ariaColSpan?: compute.ComputationOpt<string | null>
  readonly ariaCurrent?: compute.ComputationOpt<
    | 'date'
    | 'false'
    | 'location'
    | 'page'
    | 'step'
    | 'time'
    | 'true'
    | null
  >
  readonly ariaDisabled?: compute.ComputationOpt<'false' | 'true' | null>
  readonly ariaExpanded?: compute.ComputationOpt<
    'false' | 'true' | 'undefined' | null
  >
  readonly ariaHasPopup?: compute.ComputationOpt<
    | 'dialog'
    | 'false'
    | 'grid'
    | 'listbox'
    | 'menu'
    | 'tree'
    | 'true'
    | null
  >
  readonly ariaHidden?: compute.ComputationOpt<
    'false' | 'true' | 'undefined' | null
  >
  readonly ariaInvalid?: compute.ComputationOpt<string | null>
  readonly ariaKeyShortcuts?: compute.ComputationOpt<string | null>
  readonly ariaLabel?: compute.ComputationOpt<string | null>
  readonly ariaLevel?: compute.ComputationOpt<string | null>
  readonly ariaLive?: compute.ComputationOpt<
    'assertive' | 'off' | 'polite' | null
  >
  readonly ariaModal?: compute.ComputationOpt<'false' | 'true' | null>
  readonly ariaMultiLine?: compute.ComputationOpt<'false' | 'true' | null>
  readonly ariaMultiSelectable?: compute.ComputationOpt<
    'false' | 'true' | null
  >
  readonly ariaOrientation?: compute.ComputationOpt<
    'horizontal' | 'undefined' | 'vertical' | null
  >
  readonly ariaPlaceholder?: compute.ComputationOpt<string | null>
  readonly ariaPosInSet?: compute.ComputationOpt<string | null>
  readonly ariaPressed?: compute.ComputationOpt<
    'false' | 'mixed' | 'true' | 'undefined' | null
  >
  readonly ariaReadOnly?: compute.ComputationOpt<'false' | 'true' | null>
  readonly ariaRequired?: compute.ComputationOpt<'false' | 'true' | null>
  readonly ariaRoleDescription?: compute.ComputationOpt<string | null>
  readonly ariaRowCount?: compute.ComputationOpt<string | null>
  readonly ariaRowIndex?: compute.ComputationOpt<string | null>
  readonly ariaRowSpan?: compute.ComputationOpt<string | null>
  readonly ariaSelected?: compute.ComputationOpt<
    'false' | 'true' | 'undefined' | null
  >
  readonly ariaSetSize?: compute.ComputationOpt<string | null>
  readonly ariaSort?: compute.ComputationOpt<
    'ascending' | 'descending' | 'none' | 'other' | null
  >
  readonly ariaValueMax?: compute.ComputationOpt<string | null>
  readonly ariaValueMin?: compute.ComputationOpt<string | null>
  readonly ariaValueNow?: compute.ComputationOpt<string | null>
  readonly ariaValueText?: compute.ComputationOpt<string | null>
  readonly autofocus?: compute.ComputationOpt<boolean>
  readonly content?: conveyFragment.FragmentOpt<CustomContext>
  readonly id?: compute.ComputationOpt<string>
  readonly nonce?: compute.ComputationOpt<string>
  readonly onAbort?: Listener<UIEvent>
  readonly onAnimationCancel?: Listener<AnimationEvent>
  readonly onAnimationEnd?: Listener<AnimationEvent>
  readonly onAnimationIteration?: Listener<AnimationEvent>
  readonly onAnimationStart?: Listener<AnimationEvent>
  readonly onAuxClick?: Listener<MouseEvent>
  readonly onBeforeInput?: Listener<InputEvent>
  readonly onBlur?: Listener<FocusEvent>
  readonly onCanPlay?: Listener<Event>
  readonly onCanPlayThrough?: Listener<Event>
  readonly onCancel?: Listener<Event>
  readonly onChange?: Listener<Event>
  readonly onClick?: Listener<MouseEvent>
  readonly onClose?: Listener<Event>
  readonly onCompositionEnd?: Listener<CompositionEvent>
  readonly onCompositionStart?: Listener<CompositionEvent>
  readonly onCompositionUpdate?: Listener<CompositionEvent>
  readonly onContextMenu?: Listener<MouseEvent>
  readonly onCopy?: Listener<ClipboardEvent>
  readonly onCueChange?: Listener<Event>
  readonly onCut?: Listener<ClipboardEvent>
  readonly onDblClick?: Listener<MouseEvent>
  readonly onDrag?: Listener<DragEvent>
  readonly onDragEnd?: Listener<DragEvent>
  readonly onDragEnter?: Listener<DragEvent>
  readonly onDragLeave?: Listener<DragEvent>
  readonly onDragOver?: Listener<DragEvent>
  readonly onDragStart?: Listener<DragEvent>
  readonly onDrop?: Listener<DragEvent>
  readonly onDurationChange?: Listener<Event>
  readonly onEmptied?: Listener<Event>
  readonly onEnded?: Listener<Event>
  readonly onError?: Listener<ErrorEvent>
  readonly onFocus?: Listener<FocusEvent>
  readonly onFocusIn?: Listener<FocusEvent>
  readonly onFocusOut?: Listener<FocusEvent>
  readonly onFormData?: Listener<FormDataEvent>
  readonly onFullscreenChange?: Listener<Event>
  readonly onFullscreenError?: Listener<Event>
  readonly onGotPointerCapture?: Listener<PointerEvent>
  readonly onInput?: Listener<Event>
  readonly onInvalid?: Listener<Event>
  readonly onKeyDown?: Listener<KeyboardEvent>
  readonly onKeyPress?: Listener<KeyboardEvent>
  readonly onKeyUp?: Listener<KeyboardEvent>
  readonly onLoad?: Listener<Event>
  readonly onLoadStart?: Listener<Event>
  readonly onLoadedData?: Listener<Event>
  readonly onLoadedMetadata?: Listener<Event>
  readonly onLostPointercapture?: Listener<PointerEvent>
  readonly onMouseDown?: Listener<MouseEvent>
  readonly onMouseEnter?: Listener<MouseEvent>
  readonly onMouseLeave?: Listener<MouseEvent>
  readonly onMouseMove?: Listener<MouseEvent>
  readonly onMouseOut?: Listener<MouseEvent>
  readonly onMouseOver?: Listener<MouseEvent>
  readonly onMouseUp?: Listener<MouseEvent>
  readonly onPaste?: Listener<ClipboardEvent>
  readonly onPause?: Listener<Event>
  readonly onPlay?: Listener<Event>
  readonly onPlaying?: Listener<Event>
  readonly onPointerCancel?: Listener<PointerEvent>
  readonly onPointerDown?: Listener<PointerEvent>
  readonly onPointerEnter?: Listener<PointerEvent>
  readonly onPointerLeave?: Listener<PointerEvent>
  readonly onPointerMove?: Listener<PointerEvent>
  readonly onPointerOut?: Listener<PointerEvent>
  readonly onPointerOver?: Listener<PointerEvent>
  readonly onPointerUp?: Listener<PointerEvent>
  readonly onProgress?: Listener<ProgressEvent>
  readonly onRateChange?: Listener<Event>
  readonly onReset?: Listener<Event>
  readonly onResize?: Listener<UIEvent>
  readonly onScroll?: Listener<Event>
  readonly onScrollEnd?: Listener<Event>
  readonly onSecurityPolicyViolation?: Listener<SecurityPolicyViolationEvent>
  readonly onSeeked?: Listener<Event>
  readonly onSeeking?: Listener<Event>
  readonly onSelect?: Listener<Event>
  readonly onSelectStart?: Listener<Event>
  readonly onSelectionChange?: Listener<Event>
  readonly onSlotChange?: Listener<Event>
  readonly onStalled?: Listener<Event>
  readonly onSubmit?: Listener<SubmitEvent>
  readonly onSuspend?: Listener<Event>
  readonly onTimeUpdate?: Listener<Event>
  readonly onToggle?: Listener<Event>
  readonly onTouchCancel?: Listener<TouchEvent>
  readonly onTouchEnd?: Listener<TouchEvent>
  readonly onTouchMove?: Listener<TouchEvent>
  readonly onTouchStart?: Listener<TouchEvent>
  readonly onTransitionCancel?: Listener<TransitionEvent>
  readonly onTransitionEnd?: Listener<TransitionEvent>
  readonly onTransitionRun?: Listener<TransitionEvent>
  readonly onTransitionStart?: Listener<TransitionEvent>
  readonly onVolumeChange?: Listener<Event>
  readonly onWaiting?: Listener<Event>
  readonly onWebkitAnimationEnd?: Listener<Event>
  readonly onWebkitAnimationIteration?: Listener<Event>
  readonly onWebkitAnimationStart?: Listener<Event>
  readonly onWebkitTransitionEnd?: Listener<Event>
  readonly onWheel?: Listener<WheelEvent>
  readonly role?: compute.ComputationOpt<string | null>
  readonly slot?: compute.ComputationOpt<string>
  readonly tabIndex?: compute.ComputationOpt<number>
}
