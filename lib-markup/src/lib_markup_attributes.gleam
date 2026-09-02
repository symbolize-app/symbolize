import gleam/int
import gleam/option.{type Option, None, Some}
import lib_dataflow as dataflow
import lib_error
import lib_markup_fragment as fragment
import lib_styling as styling
import lib_styling_atom as atom

// JavaScript supplies the concrete DOM event object. This identity boundary
// lets each record field retain its source-specific event type while the
// fragment attribute keeps one browser listener representation.
@external(javascript, "./dom_ffi.mjs", "cast_event")
fn cast_event(event: fragment.Event) -> event_type

pub type Events {
  Events(
    on_abort: Option(Listener(fragment.UIEvent)),
    on_animation_cancel: Option(Listener(fragment.AnimationEvent)),
    on_animation_end: Option(Listener(fragment.AnimationEvent)),
    on_animation_iteration: Option(Listener(fragment.AnimationEvent)),
    on_animation_start: Option(Listener(fragment.AnimationEvent)),
    on_aux_click: Option(Listener(fragment.MouseEvent)),
    on_before_input: Option(Listener(fragment.InputEvent)),
    on_blur: Option(Listener(fragment.FocusEvent)),
    on_cancel: Option(Listener(fragment.Event)),
    on_can_play: Option(Listener(fragment.Event)),
    on_can_play_through: Option(Listener(fragment.Event)),
    on_change: Option(Listener(fragment.Event)),
    on_click: Option(Listener(fragment.MouseEvent)),
    on_close: Option(Listener(fragment.Event)),
    on_composition_end: Option(Listener(fragment.CompositionEvent)),
    on_composition_start: Option(Listener(fragment.CompositionEvent)),
    on_composition_update: Option(Listener(fragment.CompositionEvent)),
    on_context_menu: Option(Listener(fragment.MouseEvent)),
    on_copy: Option(Listener(fragment.ClipboardEvent)),
    on_cue_change: Option(Listener(fragment.Event)),
    on_cut: Option(Listener(fragment.ClipboardEvent)),
    on_dbl_click: Option(Listener(fragment.MouseEvent)),
    on_drag: Option(Listener(fragment.DragEvent)),
    on_drag_end: Option(Listener(fragment.DragEvent)),
    on_drag_enter: Option(Listener(fragment.DragEvent)),
    on_drag_leave: Option(Listener(fragment.DragEvent)),
    on_drag_over: Option(Listener(fragment.DragEvent)),
    on_drag_start: Option(Listener(fragment.DragEvent)),
    on_drop: Option(Listener(fragment.DragEvent)),
    on_duration_change: Option(Listener(fragment.Event)),
    on_emptied: Option(Listener(fragment.Event)),
    on_ended: Option(Listener(fragment.Event)),
    on_error: Option(Listener(fragment.ErrorEvent)),
    on_focus: Option(Listener(fragment.FocusEvent)),
    on_focus_in: Option(Listener(fragment.FocusEvent)),
    on_focus_out: Option(Listener(fragment.FocusEvent)),
    on_form_data: Option(Listener(fragment.FormDataEvent)),
    on_fullscreen_change: Option(Listener(fragment.Event)),
    on_fullscreen_error: Option(Listener(fragment.Event)),
    on_got_pointer_capture: Option(Listener(fragment.PointerEvent)),
    on_input: Option(Listener(fragment.Event)),
    on_invalid: Option(Listener(fragment.Event)),
    on_key_down: Option(Listener(fragment.KeyboardEvent)),
    on_key_press: Option(Listener(fragment.KeyboardEvent)),
    on_key_up: Option(Listener(fragment.KeyboardEvent)),
    on_load: Option(Listener(fragment.Event)),
    on_loaded_data: Option(Listener(fragment.Event)),
    on_loaded_metadata: Option(Listener(fragment.Event)),
    on_load_start: Option(Listener(fragment.Event)),
    on_lost_pointer_capture: Option(Listener(fragment.PointerEvent)),
    on_mouse_down: Option(Listener(fragment.MouseEvent)),
    on_mouse_enter: Option(Listener(fragment.MouseEvent)),
    on_mouse_leave: Option(Listener(fragment.MouseEvent)),
    on_mouse_move: Option(Listener(fragment.MouseEvent)),
    on_mouse_out: Option(Listener(fragment.MouseEvent)),
    on_mouse_over: Option(Listener(fragment.MouseEvent)),
    on_mouse_up: Option(Listener(fragment.MouseEvent)),
    on_paste: Option(Listener(fragment.ClipboardEvent)),
    on_pause: Option(Listener(fragment.Event)),
    on_play: Option(Listener(fragment.Event)),
    on_playing: Option(Listener(fragment.Event)),
    on_pointer_cancel: Option(Listener(fragment.PointerEvent)),
    on_pointer_down: Option(Listener(fragment.PointerEvent)),
    on_pointer_enter: Option(Listener(fragment.PointerEvent)),
    on_pointer_leave: Option(Listener(fragment.PointerEvent)),
    on_pointer_move: Option(Listener(fragment.PointerEvent)),
    on_pointer_out: Option(Listener(fragment.PointerEvent)),
    on_pointer_over: Option(Listener(fragment.PointerEvent)),
    on_pointer_up: Option(Listener(fragment.PointerEvent)),
    on_progress: Option(Listener(fragment.ProgressEvent)),
    on_rate_change: Option(Listener(fragment.Event)),
    on_reset: Option(Listener(fragment.Event)),
    on_resize: Option(Listener(fragment.UIEvent)),
    on_scroll: Option(Listener(fragment.Event)),
    on_scroll_end: Option(Listener(fragment.Event)),
    on_security_policy_violation: Option(
      Listener(fragment.SecurityPolicyViolationEvent),
    ),
    on_seeked: Option(Listener(fragment.Event)),
    on_seeking: Option(Listener(fragment.Event)),
    on_select: Option(Listener(fragment.Event)),
    on_selection_change: Option(Listener(fragment.Event)),
    on_select_start: Option(Listener(fragment.Event)),
    on_slot_change: Option(Listener(fragment.Event)),
    on_stalled: Option(Listener(fragment.Event)),
    on_submit: Option(Listener(fragment.SubmitEvent)),
    on_suspend: Option(Listener(fragment.Event)),
    on_time_update: Option(Listener(fragment.Event)),
    on_toggle: Option(Listener(fragment.Event)),
    on_touch_cancel: Option(Listener(fragment.TouchEvent)),
    on_touch_end: Option(Listener(fragment.TouchEvent)),
    on_touch_move: Option(Listener(fragment.TouchEvent)),
    on_touch_start: Option(Listener(fragment.TouchEvent)),
    on_transition_cancel: Option(Listener(fragment.TransitionEvent)),
    on_transition_end: Option(Listener(fragment.TransitionEvent)),
    on_transition_run: Option(Listener(fragment.TransitionEvent)),
    on_transition_start: Option(Listener(fragment.TransitionEvent)),
    on_volume_change: Option(Listener(fragment.Event)),
    on_waiting: Option(Listener(fragment.Event)),
    on_webkit_animation_end: Option(Listener(fragment.Event)),
    on_webkit_animation_iteration: Option(Listener(fragment.Event)),
    on_webkit_animation_start: Option(Listener(fragment.Event)),
    on_webkit_transition_end: Option(Listener(fragment.Event)),
    on_wheel: Option(Listener(fragment.WheelEvent)),
  )
}

pub type Listener(event) {
  SyncListener(fn(event) -> Nil)
  AsyncListener(fn(event) -> lib_error.Async(Nil, String))
}

pub fn sync_listener(callback: fn(event) -> Nil) -> Listener(event) {
  SyncListener(callback)
}

pub fn async_listener(
  callback: fn(event) -> lib_error.Async(Nil, String),
) -> Listener(event) {
  AsyncListener(callback)
}

pub fn events() -> Events {
  Events(
    on_abort: None,
    on_animation_cancel: None,
    on_animation_end: None,
    on_animation_iteration: None,
    on_animation_start: None,
    on_aux_click: None,
    on_before_input: None,
    on_blur: None,
    on_cancel: None,
    on_can_play: None,
    on_can_play_through: None,
    on_change: None,
    on_click: None,
    on_close: None,
    on_composition_end: None,
    on_composition_start: None,
    on_composition_update: None,
    on_context_menu: None,
    on_copy: None,
    on_cue_change: None,
    on_cut: None,
    on_dbl_click: None,
    on_drag: None,
    on_drag_end: None,
    on_drag_enter: None,
    on_drag_leave: None,
    on_drag_over: None,
    on_drag_start: None,
    on_drop: None,
    on_duration_change: None,
    on_emptied: None,
    on_ended: None,
    on_error: None,
    on_focus: None,
    on_focus_in: None,
    on_focus_out: None,
    on_form_data: None,
    on_fullscreen_change: None,
    on_fullscreen_error: None,
    on_got_pointer_capture: None,
    on_input: None,
    on_invalid: None,
    on_key_down: None,
    on_key_press: None,
    on_key_up: None,
    on_load: None,
    on_loaded_data: None,
    on_loaded_metadata: None,
    on_load_start: None,
    on_lost_pointer_capture: None,
    on_mouse_down: None,
    on_mouse_enter: None,
    on_mouse_leave: None,
    on_mouse_move: None,
    on_mouse_out: None,
    on_mouse_over: None,
    on_mouse_up: None,
    on_paste: None,
    on_pause: None,
    on_play: None,
    on_playing: None,
    on_pointer_cancel: None,
    on_pointer_down: None,
    on_pointer_enter: None,
    on_pointer_leave: None,
    on_pointer_move: None,
    on_pointer_out: None,
    on_pointer_over: None,
    on_pointer_up: None,
    on_progress: None,
    on_rate_change: None,
    on_reset: None,
    on_resize: None,
    on_scroll: None,
    on_scroll_end: None,
    on_security_policy_violation: None,
    on_seeked: None,
    on_seeking: None,
    on_select: None,
    on_selection_change: None,
    on_select_start: None,
    on_slot_change: None,
    on_stalled: None,
    on_submit: None,
    on_suspend: None,
    on_time_update: None,
    on_toggle: None,
    on_touch_cancel: None,
    on_touch_end: None,
    on_touch_move: None,
    on_touch_start: None,
    on_transition_cancel: None,
    on_transition_end: None,
    on_transition_run: None,
    on_transition_start: None,
    on_volume_change: None,
    on_waiting: None,
    on_webkit_animation_end: None,
    on_webkit_animation_iteration: None,
    on_webkit_animation_start: None,
    on_webkit_transition_end: None,
    on_wheel: None,
  )
}

pub fn common(
  id: Option(String),
  aria_disabled: Option(Bool),
  class_names: List(String),
  title: Option(String),
  nonce: Option(String),
  role: Option(String),
  slot: Option(String),
  tab_index: Option(Int),
  autofocus: Option(Bool),
) -> List(fragment.Attribute) {
  let attrs = add_string([], "id", id)
  let attrs =
    add_string(attrs, "aria-disabled", option.map(aria_disabled, bool_text))
  let attrs = add_string(attrs, "title", title)
  let attrs = add_string(attrs, "nonce", nonce)
  let attrs = add_string(attrs, "role", role)
  let attrs = add_string(attrs, "slot", slot)
  let attrs = add_int(attrs, "tabindex", tab_index)
  let attrs = add_bool(attrs, "autofocus", autofocus)
  case class_names {
    [] -> attrs
    _ -> append(attrs, [fragment.ClassAttribute(class_names)])
  }
}

pub fn common_reactive_basic(
  id: dataflow.NodeOpt(Option(String)),
  aria_disabled: dataflow.NodeOpt(Option(Bool)),
  class_names: dataflow.NodeOpt(Option(List(String))),
  title: dataflow.NodeOpt(Option(String)),
  nonce: dataflow.NodeOpt(Option(String)),
  role: dataflow.NodeOpt(Option(String)),
  slot: dataflow.NodeOpt(Option(String)),
  tab_index: dataflow.NodeOpt(Option(Int)),
  autofocus: dataflow.NodeOpt(Option(Bool)),
) -> List(fragment.Attribute) {
  [
    fragment.ReactiveOptionalStringAttribute("id", id),
    fragment.ReactiveOptionalBooleanStringAttribute(
      "aria-disabled",
      aria_disabled,
    ),
    fragment.ReactiveOptionalStringAttribute("title", title),
    fragment.ReactiveOptionalStringAttribute("nonce", nonce),
    fragment.ReactiveOptionalStringAttribute("role", role),
    fragment.ReactiveOptionalStringAttribute("slot", slot),
    fragment.ReactiveOptionalStringAttribute(
      "tabindex",
      map_optional(tab_index, int.to_string),
    ),
    fragment.ReactiveOptionalBooleanAttribute("autofocus", autofocus),
    fragment.ReactiveOptionalClassAttribute(class_names),
  ]
}

pub fn common_reactive_aria(
  aria_atomic: dataflow.NodeOpt(Option(String)),
  aria_auto_complete: dataflow.NodeOpt(Option(String)),
  aria_busy: dataflow.NodeOpt(Option(String)),
  aria_checked: dataflow.NodeOpt(Option(String)),
  aria_col_count: dataflow.NodeOpt(Option(String)),
  aria_col_index: dataflow.NodeOpt(Option(String)),
  aria_col_span: dataflow.NodeOpt(Option(String)),
  aria_current: dataflow.NodeOpt(Option(String)),
  aria_description: dataflow.NodeOpt(Option(String)),
  aria_expanded: dataflow.NodeOpt(Option(String)),
  aria_has_popup: dataflow.NodeOpt(Option(String)),
  aria_hidden: dataflow.NodeOpt(Option(String)),
  aria_invalid: dataflow.NodeOpt(Option(String)),
  aria_key_shortcuts: dataflow.NodeOpt(Option(String)),
  aria_label: dataflow.NodeOpt(Option(String)),
  aria_level: dataflow.NodeOpt(Option(String)),
  aria_live: dataflow.NodeOpt(Option(String)),
  aria_modal: dataflow.NodeOpt(Option(String)),
  aria_multi_line: dataflow.NodeOpt(Option(String)),
  aria_multi_selectable: dataflow.NodeOpt(Option(String)),
  aria_orientation: dataflow.NodeOpt(Option(String)),
  aria_placeholder: dataflow.NodeOpt(Option(String)),
  aria_pos_in_set: dataflow.NodeOpt(Option(String)),
  aria_pressed: dataflow.NodeOpt(Option(String)),
  aria_read_only: dataflow.NodeOpt(Option(String)),
  aria_required: dataflow.NodeOpt(Option(String)),
  aria_role_description: dataflow.NodeOpt(Option(String)),
  aria_row_count: dataflow.NodeOpt(Option(String)),
  aria_row_index: dataflow.NodeOpt(Option(String)),
  aria_row_span: dataflow.NodeOpt(Option(String)),
  aria_selected: dataflow.NodeOpt(Option(String)),
  aria_set_size: dataflow.NodeOpt(Option(String)),
  aria_sort: dataflow.NodeOpt(Option(String)),
  aria_value_max: dataflow.NodeOpt(Option(String)),
  aria_value_min: dataflow.NodeOpt(Option(String)),
  aria_value_now: dataflow.NodeOpt(Option(String)),
  aria_value_text: dataflow.NodeOpt(Option(String)),
  id: dataflow.NodeOpt(Option(String)),
  aria_disabled: dataflow.NodeOpt(Option(Bool)),
  class_names: dataflow.NodeOpt(Option(List(String))),
  nonce: dataflow.NodeOpt(Option(String)),
  role: dataflow.NodeOpt(Option(String)),
  slot: dataflow.NodeOpt(Option(String)),
  tab_index: dataflow.NodeOpt(Option(Int)),
  autofocus: dataflow.NodeOpt(Option(Bool)),
  events: Events,
) -> List(fragment.Attribute) {
  let attrs = [
    fragment.ReactiveOptionalStringAttribute("aria-atomic", aria_atomic),
    fragment.ReactiveOptionalStringAttribute(
      "aria-autocomplete",
      aria_auto_complete,
    ),
    fragment.ReactiveOptionalStringAttribute("aria-busy", aria_busy),
    fragment.ReactiveOptionalStringAttribute("aria-checked", aria_checked),
    fragment.ReactiveOptionalStringAttribute("aria-colcount", aria_col_count),
    fragment.ReactiveOptionalStringAttribute("aria-colindex", aria_col_index),
    fragment.ReactiveOptionalStringAttribute("aria-colspan", aria_col_span),
    fragment.ReactiveOptionalStringAttribute("aria-current", aria_current),
    fragment.ReactiveOptionalStringAttribute(
      "aria-description",
      aria_description,
    ),
    fragment.ReactiveOptionalStringAttribute("id", id),
    fragment.ReactiveOptionalBooleanStringAttribute(
      "aria-disabled",
      aria_disabled,
    ),
    fragment.ReactiveOptionalStringAttribute("aria-expanded", aria_expanded),
    fragment.ReactiveOptionalStringAttribute("aria-haspopup", aria_has_popup),
    fragment.ReactiveOptionalStringAttribute("aria-hidden", aria_hidden),
    fragment.ReactiveOptionalStringAttribute("aria-invalid", aria_invalid),
    fragment.ReactiveOptionalStringAttribute(
      "aria-keyshortcuts",
      aria_key_shortcuts,
    ),
    fragment.ReactiveOptionalStringAttribute("aria-label", aria_label),
    fragment.ReactiveOptionalStringAttribute("aria-level", aria_level),
    fragment.ReactiveOptionalStringAttribute("aria-live", aria_live),
    fragment.ReactiveOptionalStringAttribute("aria-modal", aria_modal),
    fragment.ReactiveOptionalStringAttribute("aria-multiline", aria_multi_line),
    fragment.ReactiveOptionalStringAttribute(
      "aria-multiselectable",
      aria_multi_selectable,
    ),
    fragment.ReactiveOptionalStringAttribute(
      "aria-orientation",
      aria_orientation,
    ),
    fragment.ReactiveOptionalStringAttribute(
      "aria-placeholder",
      aria_placeholder,
    ),
    fragment.ReactiveOptionalStringAttribute("aria-posinset", aria_pos_in_set),
    fragment.ReactiveOptionalStringAttribute("aria-pressed", aria_pressed),
    fragment.ReactiveOptionalStringAttribute("aria-readonly", aria_read_only),
    fragment.ReactiveOptionalStringAttribute("aria-required", aria_required),
    fragment.ReactiveOptionalStringAttribute(
      "aria-roledescription",
      aria_role_description,
    ),
    fragment.ReactiveOptionalStringAttribute("aria-rowcount", aria_row_count),
    fragment.ReactiveOptionalStringAttribute("aria-rowindex", aria_row_index),
    fragment.ReactiveOptionalStringAttribute("aria-rowspan", aria_row_span),
    fragment.ReactiveOptionalStringAttribute("aria-selected", aria_selected),
    fragment.ReactiveOptionalStringAttribute("aria-setsize", aria_set_size),
    fragment.ReactiveOptionalStringAttribute("aria-sort", aria_sort),
    fragment.ReactiveOptionalStringAttribute("aria-valuemax", aria_value_max),
    fragment.ReactiveOptionalStringAttribute("aria-valuemin", aria_value_min),
    fragment.ReactiveOptionalStringAttribute("aria-valuenow", aria_value_now),
    fragment.ReactiveOptionalStringAttribute("aria-valuetext", aria_value_text),
    fragment.ReactiveOptionalClassAttribute(class_names),
    fragment.ReactiveOptionalStringAttribute("nonce", nonce),
    fragment.ReactiveOptionalStringAttribute("role", role),
    fragment.ReactiveOptionalStringAttribute("slot", slot),
    fragment.ReactiveOptionalStringAttribute(
      "tabindex",
      map_optional(tab_index, int.to_string),
    ),
    fragment.ReactiveOptionalBooleanAttribute("autofocus", autofocus),
  ]
  add_events(attrs, events)
}

pub fn common_reactive(
  access_key: dataflow.NodeOpt(Option(String)),
  autocapitalize: dataflow.NodeOpt(Option(String)),
  content_editable: dataflow.NodeOpt(Option(String)),
  dir: dataflow.NodeOpt(Option(String)),
  draggable: dataflow.NodeOpt(Option(String)),
  enter_key_hint: dataflow.NodeOpt(Option(String)),
  hidden: dataflow.NodeOpt(Option(String)),
  inert: dataflow.NodeOpt(Option(Bool)),
  input_mode: dataflow.NodeOpt(Option(String)),
  lang: dataflow.NodeOpt(Option(String)),
  aria_atomic: dataflow.NodeOpt(Option(String)),
  aria_auto_complete: dataflow.NodeOpt(Option(String)),
  aria_busy: dataflow.NodeOpt(Option(String)),
  aria_checked: dataflow.NodeOpt(Option(String)),
  aria_col_count: dataflow.NodeOpt(Option(String)),
  aria_col_index: dataflow.NodeOpt(Option(String)),
  aria_col_span: dataflow.NodeOpt(Option(String)),
  aria_current: dataflow.NodeOpt(Option(String)),
  aria_description: dataflow.NodeOpt(Option(String)),
  aria_expanded: dataflow.NodeOpt(Option(String)),
  aria_has_popup: dataflow.NodeOpt(Option(String)),
  aria_hidden: dataflow.NodeOpt(Option(String)),
  aria_invalid: dataflow.NodeOpt(Option(String)),
  aria_key_shortcuts: dataflow.NodeOpt(Option(String)),
  aria_label: dataflow.NodeOpt(Option(String)),
  aria_level: dataflow.NodeOpt(Option(String)),
  aria_live: dataflow.NodeOpt(Option(String)),
  aria_modal: dataflow.NodeOpt(Option(String)),
  aria_multi_line: dataflow.NodeOpt(Option(String)),
  aria_multi_selectable: dataflow.NodeOpt(Option(String)),
  aria_orientation: dataflow.NodeOpt(Option(String)),
  aria_placeholder: dataflow.NodeOpt(Option(String)),
  aria_pos_in_set: dataflow.NodeOpt(Option(String)),
  aria_pressed: dataflow.NodeOpt(Option(String)),
  aria_read_only: dataflow.NodeOpt(Option(String)),
  aria_required: dataflow.NodeOpt(Option(String)),
  aria_role_description: dataflow.NodeOpt(Option(String)),
  aria_row_count: dataflow.NodeOpt(Option(String)),
  aria_row_index: dataflow.NodeOpt(Option(String)),
  aria_row_span: dataflow.NodeOpt(Option(String)),
  aria_selected: dataflow.NodeOpt(Option(String)),
  aria_set_size: dataflow.NodeOpt(Option(String)),
  aria_sort: dataflow.NodeOpt(Option(String)),
  aria_value_max: dataflow.NodeOpt(Option(String)),
  aria_value_min: dataflow.NodeOpt(Option(String)),
  aria_value_now: dataflow.NodeOpt(Option(String)),
  aria_value_text: dataflow.NodeOpt(Option(String)),
  id: dataflow.NodeOpt(Option(String)),
  aria_disabled: dataflow.NodeOpt(Option(Bool)),
  class_names: dataflow.NodeOpt(Option(List(String))),
  title: dataflow.NodeOpt(Option(String)),
  nonce: dataflow.NodeOpt(Option(String)),
  role: dataflow.NodeOpt(Option(String)),
  slot: dataflow.NodeOpt(Option(String)),
  spellcheck: dataflow.NodeOpt(Option(String)),
  tab_index: dataflow.NodeOpt(Option(Int)),
  autofocus: dataflow.NodeOpt(Option(Bool)),
  translate: dataflow.NodeOpt(Option(String)),
  events: Events,
) -> List(fragment.Attribute) {
  add_events(
    [
      fragment.ReactiveOptionalStringAttribute("accesskey", access_key),
      fragment.ReactiveOptionalStringAttribute("autocapitalize", autocapitalize),
      fragment.ReactiveOptionalStringAttribute(
        "contenteditable",
        content_editable,
      ),
      fragment.ReactiveOptionalStringAttribute("dir", dir),
      fragment.ReactiveOptionalStringAttribute("draggable", draggable),
      fragment.ReactiveOptionalStringAttribute("enterkeyhint", enter_key_hint),
      fragment.ReactiveOptionalStringAttribute("hidden", hidden),
      fragment.ReactiveOptionalBooleanAttribute("inert", inert),
      fragment.ReactiveOptionalStringAttribute("inputmode", input_mode),
      fragment.ReactiveOptionalStringAttribute("lang", lang),
      fragment.ReactiveOptionalStringAttribute("aria-atomic", aria_atomic),
      fragment.ReactiveOptionalStringAttribute(
        "aria-autocomplete",
        aria_auto_complete,
      ),
      fragment.ReactiveOptionalStringAttribute("aria-busy", aria_busy),
      fragment.ReactiveOptionalStringAttribute("aria-checked", aria_checked),
      fragment.ReactiveOptionalStringAttribute("aria-colcount", aria_col_count),
      fragment.ReactiveOptionalStringAttribute("aria-colindex", aria_col_index),
      fragment.ReactiveOptionalStringAttribute("aria-colspan", aria_col_span),
      fragment.ReactiveOptionalStringAttribute("aria-current", aria_current),
      fragment.ReactiveOptionalStringAttribute(
        "aria-description",
        aria_description,
      ),
      fragment.ReactiveOptionalStringAttribute("id", id),
      fragment.ReactiveOptionalBooleanStringAttribute(
        "aria-disabled",
        aria_disabled,
      ),
      fragment.ReactiveOptionalStringAttribute("aria-expanded", aria_expanded),
      fragment.ReactiveOptionalStringAttribute("aria-haspopup", aria_has_popup),
      fragment.ReactiveOptionalStringAttribute("aria-hidden", aria_hidden),
      fragment.ReactiveOptionalStringAttribute("aria-invalid", aria_invalid),
      fragment.ReactiveOptionalStringAttribute(
        "aria-keyshortcuts",
        aria_key_shortcuts,
      ),
      fragment.ReactiveOptionalStringAttribute("aria-label", aria_label),
      fragment.ReactiveOptionalStringAttribute("aria-level", aria_level),
      fragment.ReactiveOptionalStringAttribute("aria-live", aria_live),
      fragment.ReactiveOptionalStringAttribute("aria-modal", aria_modal),
      fragment.ReactiveOptionalStringAttribute(
        "aria-multiline",
        aria_multi_line,
      ),
      fragment.ReactiveOptionalStringAttribute(
        "aria-multiselectable",
        aria_multi_selectable,
      ),
      fragment.ReactiveOptionalStringAttribute(
        "aria-orientation",
        aria_orientation,
      ),
      fragment.ReactiveOptionalStringAttribute(
        "aria-placeholder",
        aria_placeholder,
      ),
      fragment.ReactiveOptionalStringAttribute("aria-posinset", aria_pos_in_set),
      fragment.ReactiveOptionalStringAttribute("aria-pressed", aria_pressed),
      fragment.ReactiveOptionalStringAttribute("aria-readonly", aria_read_only),
      fragment.ReactiveOptionalStringAttribute("aria-required", aria_required),
      fragment.ReactiveOptionalStringAttribute(
        "aria-roledescription",
        aria_role_description,
      ),
      fragment.ReactiveOptionalStringAttribute("aria-rowcount", aria_row_count),
      fragment.ReactiveOptionalStringAttribute("aria-rowindex", aria_row_index),
      fragment.ReactiveOptionalStringAttribute("aria-rowspan", aria_row_span),
      fragment.ReactiveOptionalStringAttribute("aria-selected", aria_selected),
      fragment.ReactiveOptionalStringAttribute("aria-setsize", aria_set_size),
      fragment.ReactiveOptionalStringAttribute("aria-sort", aria_sort),
      fragment.ReactiveOptionalStringAttribute("aria-valuemax", aria_value_max),
      fragment.ReactiveOptionalStringAttribute("aria-valuemin", aria_value_min),
      fragment.ReactiveOptionalStringAttribute("aria-valuenow", aria_value_now),
      fragment.ReactiveOptionalStringAttribute(
        "aria-valuetext",
        aria_value_text,
      ),
      fragment.ReactiveOptionalClassAttribute(class_names),
      fragment.ReactiveOptionalStringAttribute("title", title),
      fragment.ReactiveOptionalStringAttribute("nonce", nonce),
      fragment.ReactiveOptionalStringAttribute("role", role),
      fragment.ReactiveOptionalStringAttribute("slot", slot),
      fragment.ReactiveOptionalStringAttribute("spellcheck", spellcheck),
      fragment.ReactiveOptionalStringAttribute(
        "tabindex",
        dataflow.reactive(dataflow.map(
          fn(value) { option.map(value, int.to_string) },
          tab_index,
        )),
      ),
      fragment.ReactiveOptionalBooleanAttribute("autofocus", autofocus),
      fragment.ReactiveOptionalStringAttribute("translate", translate),
    ],
    events,
  )
}

pub fn map_optional(
  value: dataflow.NodeOpt(Option(a)),
  transform: fn(a) -> b,
) -> dataflow.NodeOpt(Option(b)) {
  case value {
    dataflow.Literal(value) -> dataflow.Literal(option.map(value, transform))
    dataflow.Reactive(_) ->
      dataflow.reactive(dataflow.map(
        fn(value) { option.map(value, transform) },
        value,
      ))
  }
}

pub fn add_event(
  attrs: List(fragment.Attribute),
  name: String,
  listener: Option(Listener(event_type)),
) -> List(fragment.Attribute) {
  case listener {
    None -> attrs
    Some(SyncListener(listener)) ->
      append(attrs, [
        fragment.EventAttribute(
          name,
          fragment.SyncEventListener(fn(event) { listener(cast_event(event)) }),
        ),
      ])
    Some(AsyncListener(listener)) ->
      append(attrs, [
        fragment.EventAttribute(
          name,
          fragment.AsyncEventListener(fn(event) { listener(cast_event(event)) }),
        ),
      ])
  }
}

pub fn add_events(
  attrs: List(fragment.Attribute),
  events: Events,
) -> List(fragment.Attribute) {
  let Events(
    on_abort,
    on_animation_cancel,
    on_animation_end,
    on_animation_iteration,
    on_animation_start,
    on_aux_click,
    on_before_input,
    on_blur,
    on_cancel,
    on_can_play,
    on_can_play_through,
    on_change,
    on_click,
    on_close,
    on_composition_end,
    on_composition_start,
    on_composition_update,
    on_context_menu,
    on_copy,
    on_cue_change,
    on_cut,
    on_dbl_click,
    on_drag,
    on_drag_end,
    on_drag_enter,
    on_drag_leave,
    on_drag_over,
    on_drag_start,
    on_drop,
    on_duration_change,
    on_emptied,
    on_ended,
    on_error,
    on_focus,
    on_focus_in,
    on_focus_out,
    on_form_data,
    on_fullscreen_change,
    on_fullscreen_error,
    on_got_pointer_capture,
    on_input,
    on_invalid,
    on_key_down,
    on_key_press,
    on_key_up,
    on_load,
    on_loaded_data,
    on_loaded_metadata,
    on_load_start,
    on_lost_pointer_capture,
    on_mouse_down,
    on_mouse_enter,
    on_mouse_leave,
    on_mouse_move,
    on_mouse_out,
    on_mouse_over,
    on_mouse_up,
    on_paste,
    on_pause,
    on_play,
    on_playing,
    on_pointer_cancel,
    on_pointer_down,
    on_pointer_enter,
    on_pointer_leave,
    on_pointer_move,
    on_pointer_out,
    on_pointer_over,
    on_pointer_up,
    on_progress,
    on_rate_change,
    on_reset,
    on_resize,
    on_scroll,
    on_scroll_end,
    on_security_policy_violation,
    on_seeked,
    on_seeking,
    on_select,
    on_selection_change,
    on_select_start,
    on_slot_change,
    on_stalled,
    on_submit,
    on_suspend,
    on_time_update,
    on_toggle,
    on_touch_cancel,
    on_touch_end,
    on_touch_move,
    on_touch_start,
    on_transition_cancel,
    on_transition_end,
    on_transition_run,
    on_transition_start,
    on_volume_change,
    on_waiting,
    on_webkit_animation_end,
    on_webkit_animation_iteration,
    on_webkit_animation_start,
    on_webkit_transition_end,
    on_wheel,
  ) = events
  let attrs = add_event(attrs, "abort", on_abort)
  let attrs = add_event(attrs, "animationcancel", on_animation_cancel)
  let attrs = add_event(attrs, "animationend", on_animation_end)
  let attrs = add_event(attrs, "animationiteration", on_animation_iteration)
  let attrs = add_event(attrs, "animationstart", on_animation_start)
  let attrs = add_event(attrs, "auxclick", on_aux_click)
  let attrs = add_event(attrs, "beforeinput", on_before_input)
  let attrs = add_event(attrs, "blur", on_blur)
  let attrs = add_event(attrs, "cancel", on_cancel)
  let attrs = add_event(attrs, "canplay", on_can_play)
  let attrs = add_event(attrs, "canplaythrough", on_can_play_through)
  let attrs = add_event(attrs, "change", on_change)
  let attrs = add_event(attrs, "click", on_click)
  let attrs = add_event(attrs, "close", on_close)
  let attrs = add_event(attrs, "compositionend", on_composition_end)
  let attrs = add_event(attrs, "compositionstart", on_composition_start)
  let attrs = add_event(attrs, "compositionupdate", on_composition_update)
  let attrs = add_event(attrs, "contextmenu", on_context_menu)
  let attrs = add_event(attrs, "copy", on_copy)
  let attrs = add_event(attrs, "cuechange", on_cue_change)
  let attrs = add_event(attrs, "cut", on_cut)
  let attrs = add_event(attrs, "dblclick", on_dbl_click)
  let attrs = add_event(attrs, "drag", on_drag)
  let attrs = add_event(attrs, "dragend", on_drag_end)
  let attrs = add_event(attrs, "dragenter", on_drag_enter)
  let attrs = add_event(attrs, "dragleave", on_drag_leave)
  let attrs = add_event(attrs, "dragover", on_drag_over)
  let attrs = add_event(attrs, "dragstart", on_drag_start)
  let attrs = add_event(attrs, "drop", on_drop)
  let attrs = add_event(attrs, "durationchange", on_duration_change)
  let attrs = add_event(attrs, "emptied", on_emptied)
  let attrs = add_event(attrs, "ended", on_ended)
  let attrs = add_event(attrs, "error", on_error)
  let attrs = add_event(attrs, "focus", on_focus)
  let attrs = add_event(attrs, "focusin", on_focus_in)
  let attrs = add_event(attrs, "focusout", on_focus_out)
  let attrs = add_event(attrs, "formdata", on_form_data)
  let attrs = add_event(attrs, "fullscreenchange", on_fullscreen_change)
  let attrs = add_event(attrs, "fullscreenerror", on_fullscreen_error)
  let attrs = add_event(attrs, "gotpointercapture", on_got_pointer_capture)
  let attrs = add_event(attrs, "input", on_input)
  let attrs = add_event(attrs, "invalid", on_invalid)
  let attrs = add_event(attrs, "keydown", on_key_down)
  let attrs = add_event(attrs, "keypress", on_key_press)
  let attrs = add_event(attrs, "keyup", on_key_up)
  let attrs = add_event(attrs, "load", on_load)
  let attrs = add_event(attrs, "loadeddata", on_loaded_data)
  let attrs = add_event(attrs, "loadedmetadata", on_loaded_metadata)
  let attrs = add_event(attrs, "loadstart", on_load_start)
  let attrs = add_event(attrs, "lostpointercapture", on_lost_pointer_capture)
  let attrs = add_event(attrs, "mousedown", on_mouse_down)
  let attrs = add_event(attrs, "mouseenter", on_mouse_enter)
  let attrs = add_event(attrs, "mouseleave", on_mouse_leave)
  let attrs = add_event(attrs, "mousemove", on_mouse_move)
  let attrs = add_event(attrs, "mouseout", on_mouse_out)
  let attrs = add_event(attrs, "mouseover", on_mouse_over)
  let attrs = add_event(attrs, "mouseup", on_mouse_up)
  let attrs = add_event(attrs, "paste", on_paste)
  let attrs = add_event(attrs, "pause", on_pause)
  let attrs = add_event(attrs, "play", on_play)
  let attrs = add_event(attrs, "playing", on_playing)
  let attrs = add_event(attrs, "pointercancel", on_pointer_cancel)
  let attrs = add_event(attrs, "pointerdown", on_pointer_down)
  let attrs = add_event(attrs, "pointerenter", on_pointer_enter)
  let attrs = add_event(attrs, "pointerleave", on_pointer_leave)
  let attrs = add_event(attrs, "pointermove", on_pointer_move)
  let attrs = add_event(attrs, "pointerout", on_pointer_out)
  let attrs = add_event(attrs, "pointerover", on_pointer_over)
  let attrs = add_event(attrs, "pointerup", on_pointer_up)
  let attrs = add_event(attrs, "progress", on_progress)
  let attrs = add_event(attrs, "ratechange", on_rate_change)
  let attrs = add_event(attrs, "reset", on_reset)
  let attrs = add_event(attrs, "resize", on_resize)
  let attrs = add_event(attrs, "scroll", on_scroll)
  let attrs = add_event(attrs, "scrollend", on_scroll_end)
  let attrs =
    add_event(attrs, "securitypolicyviolation", on_security_policy_violation)
  let attrs = add_event(attrs, "seeked", on_seeked)
  let attrs = add_event(attrs, "seeking", on_seeking)
  let attrs = add_event(attrs, "select", on_select)
  let attrs = add_event(attrs, "selectionchange", on_selection_change)
  let attrs = add_event(attrs, "selectstart", on_select_start)
  let attrs = add_event(attrs, "slotchange", on_slot_change)
  let attrs = add_event(attrs, "stalled", on_stalled)
  let attrs = add_event(attrs, "submit", on_submit)
  let attrs = add_event(attrs, "suspend", on_suspend)
  let attrs = add_event(attrs, "timeupdate", on_time_update)
  let attrs = add_event(attrs, "toggle", on_toggle)
  let attrs = add_event(attrs, "touchcancel", on_touch_cancel)
  let attrs = add_event(attrs, "touchend", on_touch_end)
  let attrs = add_event(attrs, "touchmove", on_touch_move)
  let attrs = add_event(attrs, "touchstart", on_touch_start)
  let attrs = add_event(attrs, "transitioncancel", on_transition_cancel)
  let attrs = add_event(attrs, "transitionend", on_transition_end)
  let attrs = add_event(attrs, "transitionrun", on_transition_run)
  let attrs = add_event(attrs, "transitionstart", on_transition_start)
  let attrs = add_event(attrs, "volumechange", on_volume_change)
  let attrs = add_event(attrs, "waiting", on_waiting)
  let attrs = add_event(attrs, "webkitanimationend", on_webkit_animation_end)
  let attrs =
    add_event(attrs, "webkitanimationiteration", on_webkit_animation_iteration)
  let attrs =
    add_event(attrs, "webkitanimationstart", on_webkit_animation_start)
  let attrs = add_event(attrs, "webkittransitionend", on_webkit_transition_end)
  add_event(attrs, "wheel", on_wheel)
}

pub fn add_on_add(
  attrs: List(fragment.Attribute),
  callback: Option(fragment.OnAdd),
) -> List(fragment.Attribute) {
  case callback {
    None -> attrs
    Some(callback) -> append(attrs, [fragment.OnAddAttribute(callback)])
  }
}

pub fn add_style(
  attrs: List(fragment.Attribute),
  value: dataflow.NodeOpt(styling.AtomOpt),
) -> List(fragment.Attribute) {
  case value {
    dataflow.Literal(atom.Empty) -> attrs
    _ -> append(attrs, [fragment.StyleAttribute(value)])
  }
}

pub fn add_string(
  attrs: List(fragment.Attribute),
  name: String,
  value: Option(String),
) -> List(fragment.Attribute) {
  case value {
    None -> attrs
    Some(value) -> append(attrs, [fragment.StringAttribute(name, value)])
  }
}

pub fn add_string_list(
  attrs: List(fragment.Attribute),
  name: String,
  values: List(String),
) -> List(fragment.Attribute) {
  case values {
    [] -> attrs
    _ -> add_string(attrs, name, Some(join(" ", values)))
  }
}

pub fn add_string_list_node(
  attrs: List(fragment.Attribute),
  name: String,
  values: dataflow.NodeOpt(Option(List(String))),
) -> List(fragment.Attribute) {
  append(attrs, [
    fragment.ReactiveOptionalStringAttribute(
      name,
      map_optional(values, fn(values) { join(" ", values) }),
    ),
  ])
}

pub fn add_int(
  attrs: List(fragment.Attribute),
  name: String,
  value: Option(Int),
) -> List(fragment.Attribute) {
  add_string(attrs, name, option.map(value, int.to_string))
}

pub fn add_float(
  attrs: List(fragment.Attribute),
  name: String,
  value: Option(Float),
  number_text: fn(Float) -> String,
) -> List(fragment.Attribute) {
  add_string(attrs, name, option.map(value, number_text))
}

pub fn add_bool(
  attrs: List(fragment.Attribute),
  name: String,
  value: Option(Bool),
) -> List(fragment.Attribute) {
  case value {
    Some(True) -> append(attrs, [fragment.BooleanAttribute(name, True)])
    _ -> attrs
  }
}

pub fn bool_text(value: Bool) -> String {
  case value {
    True -> "true"
    False -> "false"
  }
}

fn append(first: List(a), second: List(a)) -> List(a) {
  case first {
    [] -> second
    [head, ..tail] -> [head, ..append(tail, second)]
  }
}

fn join(separator: String, values: List(String)) -> String {
  case values {
    [] -> ""
    [first, ..rest] -> join_nonempty(separator, rest, first)
  }
}

fn join_nonempty(
  separator: String,
  values: List(String),
  output: String,
) -> String {
  case values {
    [] -> output
    [first, ..rest] ->
      join_nonempty(separator, rest, output <> separator <> first)
  }
}
