import gleam/list
import gleam/option.{type Option, None}
import lib_dataflow as dataflow
import lib_markup_aria_text as aria_text
import lib_markup_attributes as attr
import lib_markup_data as data
import lib_markup_fragment as fragment
import lib_markup_html as html
import lib_styling as styling_api
import lib_styling_data as styling

pub type WindowEvents {
  WindowEvents(
    on_after_print: Option(attr.Listener(fragment.Event)),
    on_before_print: Option(attr.Listener(fragment.Event)),
    on_before_unload: Option(attr.Listener(fragment.Event)),
    on_gamepad_connected: Option(attr.Listener(fragment.GamepadEvent)),
    on_gamepad_disconnected: Option(attr.Listener(fragment.GamepadEvent)),
    on_hash_change: Option(attr.Listener(fragment.HashChangeEvent)),
    on_language_change: Option(attr.Listener(fragment.Event)),
    on_message: Option(attr.Listener(fragment.MessageEvent)),
    on_message_error: Option(attr.Listener(fragment.MessageEvent)),
    on_offline: Option(attr.Listener(fragment.Event)),
    on_online: Option(attr.Listener(fragment.Event)),
    on_page_hide: Option(attr.Listener(fragment.PageTransitionEvent)),
    on_page_show: Option(attr.Listener(fragment.PageTransitionEvent)),
    on_pop_state: Option(attr.Listener(fragment.PopStateEvent)),
    on_rejection_handled: Option(attr.Listener(fragment.PromiseRejectionEvent)),
    on_storage: Option(attr.Listener(fragment.StorageEvent)),
    on_unhandled_rejection: Option(
      attr.Listener(fragment.PromiseRejectionEvent),
    ),
    on_unload: Option(attr.Listener(fragment.Event)),
  )
}

pub fn window_events() -> WindowEvents {
  WindowEvents(
    on_after_print: None,
    on_before_print: None,
    on_before_unload: None,
    on_gamepad_connected: None,
    on_gamepad_disconnected: None,
    on_hash_change: None,
    on_language_change: None,
    on_message: None,
    on_message_error: None,
    on_offline: None,
    on_online: None,
    on_page_hide: None,
    on_page_show: None,
    on_pop_state: None,
    on_rejection_handled: None,
    on_storage: None,
    on_unhandled_rejection: None,
    on_unload: None,
  )
}

pub type GAttrs {
  GAttrs(
    aria_atomic: dataflow.NodeOpt(Option(Bool)),
    aria_auto_complete: dataflow.NodeOpt(Option(html.AriaAutoComplete)),
    aria_busy: dataflow.NodeOpt(Option(Bool)),
    aria_checked: dataflow.NodeOpt(Option(html.AriaBoolMixedUndefined)),
    aria_col_count: dataflow.NodeOpt(Option(String)),
    aria_col_index: dataflow.NodeOpt(Option(String)),
    aria_col_span: dataflow.NodeOpt(Option(String)),
    aria_current: dataflow.NodeOpt(Option(html.AriaCurrent)),
    aria_description: dataflow.NodeOpt(Option(String)),
    aria_expanded: dataflow.NodeOpt(Option(html.AriaBoolUndefined)),
    aria_has_popup: dataflow.NodeOpt(Option(html.AriaHasPopup)),
    aria_hidden: dataflow.NodeOpt(Option(html.AriaBoolUndefined)),
    aria_invalid: dataflow.NodeOpt(Option(String)),
    aria_key_shortcuts: dataflow.NodeOpt(Option(String)),
    aria_label: dataflow.NodeOpt(Option(String)),
    aria_level: dataflow.NodeOpt(Option(String)),
    aria_live: dataflow.NodeOpt(Option(html.AriaLive)),
    aria_modal: dataflow.NodeOpt(Option(Bool)),
    aria_multi_line: dataflow.NodeOpt(Option(Bool)),
    aria_multi_selectable: dataflow.NodeOpt(Option(Bool)),
    aria_orientation: dataflow.NodeOpt(Option(html.AriaOrientation)),
    aria_placeholder: dataflow.NodeOpt(Option(String)),
    aria_pos_in_set: dataflow.NodeOpt(Option(String)),
    aria_pressed: dataflow.NodeOpt(Option(html.AriaBoolMixedUndefined)),
    aria_read_only: dataflow.NodeOpt(Option(Bool)),
    aria_required: dataflow.NodeOpt(Option(Bool)),
    aria_role_description: dataflow.NodeOpt(Option(String)),
    aria_row_count: dataflow.NodeOpt(Option(String)),
    aria_row_index: dataflow.NodeOpt(Option(String)),
    aria_row_span: dataflow.NodeOpt(Option(String)),
    aria_selected: dataflow.NodeOpt(Option(html.AriaBoolUndefined)),
    aria_set_size: dataflow.NodeOpt(Option(String)),
    aria_sort: dataflow.NodeOpt(Option(html.AriaSort)),
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
    style: dataflow.NodeOpt(styling_api.AtomOpt),
    events: attr.Events,
    on_add: Option(fragment.OnAdd),
    required_extensions: dataflow.NodeOpt(Option(List(String))),
    system_language: dataflow.NodeOpt(Option(List(String))),
    content: fragment.FragmentInput,
  )
}

pub fn g_attrs() -> GAttrs {
  GAttrs(
    aria_atomic: dataflow.literal(None),
    aria_auto_complete: dataflow.literal(None),
    aria_busy: dataflow.literal(None),
    aria_checked: dataflow.literal(None),
    aria_col_count: dataflow.literal(None),
    aria_col_index: dataflow.literal(None),
    aria_col_span: dataflow.literal(None),
    aria_current: dataflow.literal(None),
    aria_description: dataflow.literal(None),
    aria_expanded: dataflow.literal(None),
    aria_has_popup: dataflow.literal(None),
    aria_hidden: dataflow.literal(None),
    aria_invalid: dataflow.literal(None),
    aria_key_shortcuts: dataflow.literal(None),
    aria_label: dataflow.literal(None),
    aria_level: dataflow.literal(None),
    aria_live: dataflow.literal(None),
    aria_modal: dataflow.literal(None),
    aria_multi_line: dataflow.literal(None),
    aria_multi_selectable: dataflow.literal(None),
    aria_orientation: dataflow.literal(None),
    aria_placeholder: dataflow.literal(None),
    aria_pos_in_set: dataflow.literal(None),
    aria_pressed: dataflow.literal(None),
    aria_read_only: dataflow.literal(None),
    aria_required: dataflow.literal(None),
    aria_role_description: dataflow.literal(None),
    aria_row_count: dataflow.literal(None),
    aria_row_index: dataflow.literal(None),
    aria_row_span: dataflow.literal(None),
    aria_selected: dataflow.literal(None),
    aria_set_size: dataflow.literal(None),
    aria_sort: dataflow.literal(None),
    aria_value_max: dataflow.literal(None),
    aria_value_min: dataflow.literal(None),
    aria_value_now: dataflow.literal(None),
    aria_value_text: dataflow.literal(None),
    id: dataflow.literal(None),
    aria_disabled: dataflow.literal(None),
    class_names: dataflow.literal(None),
    nonce: dataflow.literal(None),
    role: dataflow.literal(None),
    slot: dataflow.literal(None),
    tab_index: dataflow.literal(None),
    autofocus: dataflow.literal(None),
    style: dataflow.literal(styling_api.empty()),
    events: attr.events(),
    on_add: None,
    required_extensions: dataflow.literal(None),
    system_language: dataflow.literal(None),
    content: fragment.EmptyInput,
  )
}

pub fn g(attrs: GAttrs) -> fragment.Fragment {
  let GAttrs(
    aria_atomic,
    aria_auto_complete,
    aria_busy,
    aria_checked,
    aria_col_count,
    aria_col_index,
    aria_col_span,
    aria_current,
    aria_description,
    aria_expanded,
    aria_has_popup,
    aria_hidden,
    aria_invalid,
    aria_key_shortcuts,
    aria_label,
    aria_level,
    aria_live,
    aria_modal,
    aria_multi_line,
    aria_multi_selectable,
    aria_orientation,
    aria_placeholder,
    aria_pos_in_set,
    aria_pressed,
    aria_read_only,
    aria_required,
    aria_role_description,
    aria_row_count,
    aria_row_index,
    aria_row_span,
    aria_selected,
    aria_set_size,
    aria_sort,
    aria_value_max,
    aria_value_min,
    aria_value_now,
    aria_value_text,
    id,
    aria_disabled,
    class_names,
    nonce,
    role,
    slot,
    tab_index,
    autofocus,
    style,
    events,
    on_add,
    required_extensions,
    system_language,
    content,
  ) = attrs
  let attrs =
    attr.common_reactive_aria(
      attr.map_optional(aria_atomic, attr.bool_text),
      attr.map_optional(aria_auto_complete, aria_text.auto_complete),
      attr.map_optional(aria_busy, attr.bool_text),
      attr.map_optional(aria_checked, aria_text.bool_mixed_undefined),
      aria_col_count,
      aria_col_index,
      aria_col_span,
      attr.map_optional(aria_current, aria_text.current),
      aria_description,
      attr.map_optional(aria_expanded, aria_text.bool_undefined),
      attr.map_optional(aria_has_popup, aria_text.has_popup),
      attr.map_optional(aria_hidden, aria_text.bool_undefined),
      aria_invalid,
      aria_key_shortcuts,
      aria_label,
      aria_level,
      attr.map_optional(aria_live, aria_text.live),
      attr.map_optional(aria_modal, attr.bool_text),
      attr.map_optional(aria_multi_line, attr.bool_text),
      attr.map_optional(aria_multi_selectable, attr.bool_text),
      attr.map_optional(aria_orientation, aria_text.orientation),
      aria_placeholder,
      aria_pos_in_set,
      attr.map_optional(aria_pressed, aria_text.bool_mixed_undefined),
      attr.map_optional(aria_read_only, attr.bool_text),
      attr.map_optional(aria_required, attr.bool_text),
      aria_role_description,
      aria_row_count,
      aria_row_index,
      aria_row_span,
      attr.map_optional(aria_selected, aria_text.bool_undefined),
      aria_set_size,
      attr.map_optional(aria_sort, aria_text.sort),
      aria_value_max,
      aria_value_min,
      aria_value_now,
      aria_value_text,
      id,
      aria_disabled,
      class_names,
      nonce,
      role,
      slot,
      tab_index,
      autofocus,
      events,
    )
  let attrs = attr.add_style(attrs, style)
  let attrs = attr.add_events(attrs, events)
  let attrs = attr.add_on_add(attrs, on_add)
  let attrs =
    attr.add_string_list_node(attrs, "requiredExtensions", required_extensions)
  let attrs =
    attr.add_string_list_node(attrs, "systemLanguage", system_language)
  fragment.element_in(fragment.Svg, "g", attrs, content)
}

pub type RectAttrs {
  RectAttrs(
    aria_atomic: dataflow.NodeOpt(Option(Bool)),
    aria_auto_complete: dataflow.NodeOpt(Option(html.AriaAutoComplete)),
    aria_busy: dataflow.NodeOpt(Option(Bool)),
    aria_checked: dataflow.NodeOpt(Option(html.AriaBoolMixedUndefined)),
    aria_col_count: dataflow.NodeOpt(Option(String)),
    aria_col_index: dataflow.NodeOpt(Option(String)),
    aria_col_span: dataflow.NodeOpt(Option(String)),
    aria_current: dataflow.NodeOpt(Option(html.AriaCurrent)),
    aria_description: dataflow.NodeOpt(Option(String)),
    aria_expanded: dataflow.NodeOpt(Option(html.AriaBoolUndefined)),
    aria_has_popup: dataflow.NodeOpt(Option(html.AriaHasPopup)),
    aria_hidden: dataflow.NodeOpt(Option(html.AriaBoolUndefined)),
    aria_invalid: dataflow.NodeOpt(Option(String)),
    aria_key_shortcuts: dataflow.NodeOpt(Option(String)),
    aria_label: dataflow.NodeOpt(Option(String)),
    aria_level: dataflow.NodeOpt(Option(String)),
    aria_live: dataflow.NodeOpt(Option(html.AriaLive)),
    aria_modal: dataflow.NodeOpt(Option(Bool)),
    aria_multi_line: dataflow.NodeOpt(Option(Bool)),
    aria_multi_selectable: dataflow.NodeOpt(Option(Bool)),
    aria_orientation: dataflow.NodeOpt(Option(html.AriaOrientation)),
    aria_placeholder: dataflow.NodeOpt(Option(String)),
    aria_pos_in_set: dataflow.NodeOpt(Option(String)),
    aria_pressed: dataflow.NodeOpt(Option(html.AriaBoolMixedUndefined)),
    aria_read_only: dataflow.NodeOpt(Option(Bool)),
    aria_required: dataflow.NodeOpt(Option(Bool)),
    aria_role_description: dataflow.NodeOpt(Option(String)),
    aria_row_count: dataflow.NodeOpt(Option(String)),
    aria_row_index: dataflow.NodeOpt(Option(String)),
    aria_row_span: dataflow.NodeOpt(Option(String)),
    aria_selected: dataflow.NodeOpt(Option(html.AriaBoolUndefined)),
    aria_set_size: dataflow.NodeOpt(Option(String)),
    aria_sort: dataflow.NodeOpt(Option(html.AriaSort)),
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
    style: dataflow.NodeOpt(styling_api.AtomOpt),
    events: attr.Events,
    on_add: Option(fragment.OnAdd),
    required_extensions: dataflow.NodeOpt(Option(List(String))),
    system_language: dataflow.NodeOpt(Option(List(String))),
    height: dataflow.NodeOpt(Option(data.SvgLengthPctOpt)),
    path_length: dataflow.NodeOpt(Option(Float)),
    rx: dataflow.NodeOpt(Option(data.SvgLengthPctOpt)),
    ry: dataflow.NodeOpt(Option(data.SvgLengthPctOpt)),
    width: dataflow.NodeOpt(Option(data.SvgLengthPctOpt)),
    x: dataflow.NodeOpt(Option(data.SvgLengthPctOpt)),
    y: dataflow.NodeOpt(Option(data.SvgLengthPctOpt)),
    content: fragment.FragmentInput,
  )
}

pub fn rect_attrs() -> RectAttrs {
  RectAttrs(
    aria_atomic: dataflow.literal(None),
    aria_auto_complete: dataflow.literal(None),
    aria_busy: dataflow.literal(None),
    aria_checked: dataflow.literal(None),
    aria_col_count: dataflow.literal(None),
    aria_col_index: dataflow.literal(None),
    aria_col_span: dataflow.literal(None),
    aria_current: dataflow.literal(None),
    aria_description: dataflow.literal(None),
    aria_expanded: dataflow.literal(None),
    aria_has_popup: dataflow.literal(None),
    aria_hidden: dataflow.literal(None),
    aria_invalid: dataflow.literal(None),
    aria_key_shortcuts: dataflow.literal(None),
    aria_label: dataflow.literal(None),
    aria_level: dataflow.literal(None),
    aria_live: dataflow.literal(None),
    aria_modal: dataflow.literal(None),
    aria_multi_line: dataflow.literal(None),
    aria_multi_selectable: dataflow.literal(None),
    aria_orientation: dataflow.literal(None),
    aria_placeholder: dataflow.literal(None),
    aria_pos_in_set: dataflow.literal(None),
    aria_pressed: dataflow.literal(None),
    aria_read_only: dataflow.literal(None),
    aria_required: dataflow.literal(None),
    aria_role_description: dataflow.literal(None),
    aria_row_count: dataflow.literal(None),
    aria_row_index: dataflow.literal(None),
    aria_row_span: dataflow.literal(None),
    aria_selected: dataflow.literal(None),
    aria_set_size: dataflow.literal(None),
    aria_sort: dataflow.literal(None),
    aria_value_max: dataflow.literal(None),
    aria_value_min: dataflow.literal(None),
    aria_value_now: dataflow.literal(None),
    aria_value_text: dataflow.literal(None),
    id: dataflow.literal(None),
    aria_disabled: dataflow.literal(None),
    class_names: dataflow.literal(None),
    nonce: dataflow.literal(None),
    role: dataflow.literal(None),
    slot: dataflow.literal(None),
    tab_index: dataflow.literal(None),
    autofocus: dataflow.literal(None),
    style: dataflow.literal(styling_api.empty()),
    events: attr.events(),
    on_add: None,
    required_extensions: dataflow.literal(None),
    system_language: dataflow.literal(None),
    height: dataflow.literal(None),
    path_length: dataflow.literal(None),
    rx: dataflow.literal(None),
    ry: dataflow.literal(None),
    width: dataflow.literal(None),
    x: dataflow.literal(None),
    y: dataflow.literal(None),
    content: fragment.EmptyInput,
  )
}

pub fn rect(attrs: RectAttrs) -> fragment.Fragment {
  let RectAttrs(
    aria_atomic,
    aria_auto_complete,
    aria_busy,
    aria_checked,
    aria_col_count,
    aria_col_index,
    aria_col_span,
    aria_current,
    aria_description,
    aria_expanded,
    aria_has_popup,
    aria_hidden,
    aria_invalid,
    aria_key_shortcuts,
    aria_label,
    aria_level,
    aria_live,
    aria_modal,
    aria_multi_line,
    aria_multi_selectable,
    aria_orientation,
    aria_placeholder,
    aria_pos_in_set,
    aria_pressed,
    aria_read_only,
    aria_required,
    aria_role_description,
    aria_row_count,
    aria_row_index,
    aria_row_span,
    aria_selected,
    aria_set_size,
    aria_sort,
    aria_value_max,
    aria_value_min,
    aria_value_now,
    aria_value_text,
    id,
    aria_disabled,
    class_names,
    nonce,
    role,
    slot,
    tab_index,
    autofocus,
    style,
    events,
    on_add,
    required_extensions,
    system_language,
    height,
    path_length,
    rx,
    ry,
    width,
    x,
    y,
    content,
  ) = attrs
  let attrs =
    attr.common_reactive_aria(
      attr.map_optional(aria_atomic, attr.bool_text),
      attr.map_optional(aria_auto_complete, aria_text.auto_complete),
      attr.map_optional(aria_busy, attr.bool_text),
      attr.map_optional(aria_checked, aria_text.bool_mixed_undefined),
      aria_col_count,
      aria_col_index,
      aria_col_span,
      attr.map_optional(aria_current, aria_text.current),
      aria_description,
      attr.map_optional(aria_expanded, aria_text.bool_undefined),
      attr.map_optional(aria_has_popup, aria_text.has_popup),
      attr.map_optional(aria_hidden, aria_text.bool_undefined),
      aria_invalid,
      aria_key_shortcuts,
      aria_label,
      aria_level,
      attr.map_optional(aria_live, aria_text.live),
      attr.map_optional(aria_modal, attr.bool_text),
      attr.map_optional(aria_multi_line, attr.bool_text),
      attr.map_optional(aria_multi_selectable, attr.bool_text),
      attr.map_optional(aria_orientation, aria_text.orientation),
      aria_placeholder,
      aria_pos_in_set,
      attr.map_optional(aria_pressed, aria_text.bool_mixed_undefined),
      attr.map_optional(aria_read_only, attr.bool_text),
      attr.map_optional(aria_required, attr.bool_text),
      aria_role_description,
      aria_row_count,
      aria_row_index,
      aria_row_span,
      attr.map_optional(aria_selected, aria_text.bool_undefined),
      aria_set_size,
      attr.map_optional(aria_sort, aria_text.sort),
      aria_value_max,
      aria_value_min,
      aria_value_now,
      aria_value_text,
      id,
      aria_disabled,
      class_names,
      nonce,
      role,
      slot,
      tab_index,
      autofocus,
      events,
    )
  let attrs = attr.add_style(attrs, style)
  let attrs = attr.add_events(attrs, events)
  let attrs = attr.add_on_add(attrs, on_add)
  let attrs =
    attr.add_string_list_node(attrs, "requiredExtensions", required_extensions)
  let attrs =
    attr.add_string_list_node(attrs, "systemLanguage", system_language)
  let attrs =
    list.append(attrs, [
      fragment.ReactiveOptionalStringAttribute(
        "height",
        attr.map_optional(height, value_text),
      ),
    ])
  let attrs =
    list.append(attrs, [
      fragment.ReactiveOptionalStringAttribute(
        "pathLength",
        attr.map_optional(path_length, styling.number_text),
      ),
    ])
  let attrs =
    list.append(attrs, [
      fragment.ReactiveOptionalStringAttribute(
        "rx",
        attr.map_optional(rx, value_text),
      ),
    ])
  let attrs =
    list.append(attrs, [
      fragment.ReactiveOptionalStringAttribute(
        "ry",
        attr.map_optional(ry, value_text),
      ),
    ])
  let attrs =
    list.append(attrs, [
      fragment.ReactiveOptionalStringAttribute(
        "width",
        attr.map_optional(width, value_text),
      ),
    ])
  let attrs =
    list.append(attrs, [
      fragment.ReactiveOptionalStringAttribute(
        "x",
        attr.map_optional(x, value_text),
      ),
    ])
  let attrs =
    list.append(attrs, [
      fragment.ReactiveOptionalStringAttribute(
        "y",
        attr.map_optional(y, value_text),
      ),
    ])
  fragment.element_in(fragment.Svg, "rect", attrs, content)
}

pub type SvgAttrs {
  SvgAttrs(
    aria_atomic: dataflow.NodeOpt(Option(Bool)),
    aria_auto_complete: dataflow.NodeOpt(Option(html.AriaAutoComplete)),
    aria_busy: dataflow.NodeOpt(Option(Bool)),
    aria_checked: dataflow.NodeOpt(Option(html.AriaBoolMixedUndefined)),
    aria_col_count: dataflow.NodeOpt(Option(String)),
    aria_col_index: dataflow.NodeOpt(Option(String)),
    aria_col_span: dataflow.NodeOpt(Option(String)),
    aria_current: dataflow.NodeOpt(Option(html.AriaCurrent)),
    aria_description: dataflow.NodeOpt(Option(String)),
    aria_expanded: dataflow.NodeOpt(Option(html.AriaBoolUndefined)),
    aria_has_popup: dataflow.NodeOpt(Option(html.AriaHasPopup)),
    aria_hidden: dataflow.NodeOpt(Option(html.AriaBoolUndefined)),
    aria_invalid: dataflow.NodeOpt(Option(String)),
    aria_key_shortcuts: dataflow.NodeOpt(Option(String)),
    aria_label: dataflow.NodeOpt(Option(String)),
    aria_level: dataflow.NodeOpt(Option(String)),
    aria_live: dataflow.NodeOpt(Option(html.AriaLive)),
    aria_modal: dataflow.NodeOpt(Option(Bool)),
    aria_multi_line: dataflow.NodeOpt(Option(Bool)),
    aria_multi_selectable: dataflow.NodeOpt(Option(Bool)),
    aria_orientation: dataflow.NodeOpt(Option(html.AriaOrientation)),
    aria_placeholder: dataflow.NodeOpt(Option(String)),
    aria_pos_in_set: dataflow.NodeOpt(Option(String)),
    aria_pressed: dataflow.NodeOpt(Option(html.AriaBoolMixedUndefined)),
    aria_read_only: dataflow.NodeOpt(Option(Bool)),
    aria_required: dataflow.NodeOpt(Option(Bool)),
    aria_role_description: dataflow.NodeOpt(Option(String)),
    aria_row_count: dataflow.NodeOpt(Option(String)),
    aria_row_index: dataflow.NodeOpt(Option(String)),
    aria_row_span: dataflow.NodeOpt(Option(String)),
    aria_selected: dataflow.NodeOpt(Option(html.AriaBoolUndefined)),
    aria_set_size: dataflow.NodeOpt(Option(String)),
    aria_sort: dataflow.NodeOpt(Option(html.AriaSort)),
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
    style: dataflow.NodeOpt(styling_api.AtomOpt),
    events: attr.Events,
    window_events: WindowEvents,
    on_add: Option(fragment.OnAdd),
    required_extensions: dataflow.NodeOpt(Option(List(String))),
    system_language: dataflow.NodeOpt(Option(List(String))),
    height: dataflow.NodeOpt(Option(data.SvgLengthPctOpt)),
    preserve_aspect_ratio: dataflow.NodeOpt(
      Option(data.SvgPreserveAspectRatioOpt),
    ),
    view_box: dataflow.NodeOpt(Option(data.Rect)),
    width: dataflow.NodeOpt(Option(data.SvgLengthPctOpt)),
    x: dataflow.NodeOpt(Option(data.SvgLengthPctOpt)),
    y: dataflow.NodeOpt(Option(data.SvgLengthPctOpt)),
    content: fragment.FragmentInput,
  )
}

pub fn svg_attrs() -> SvgAttrs {
  SvgAttrs(
    aria_atomic: dataflow.literal(None),
    aria_auto_complete: dataflow.literal(None),
    aria_busy: dataflow.literal(None),
    aria_checked: dataflow.literal(None),
    aria_col_count: dataflow.literal(None),
    aria_col_index: dataflow.literal(None),
    aria_col_span: dataflow.literal(None),
    aria_current: dataflow.literal(None),
    aria_description: dataflow.literal(None),
    aria_expanded: dataflow.literal(None),
    aria_has_popup: dataflow.literal(None),
    aria_hidden: dataflow.literal(None),
    aria_invalid: dataflow.literal(None),
    aria_key_shortcuts: dataflow.literal(None),
    aria_label: dataflow.literal(None),
    aria_level: dataflow.literal(None),
    aria_live: dataflow.literal(None),
    aria_modal: dataflow.literal(None),
    aria_multi_line: dataflow.literal(None),
    aria_multi_selectable: dataflow.literal(None),
    aria_orientation: dataflow.literal(None),
    aria_placeholder: dataflow.literal(None),
    aria_pos_in_set: dataflow.literal(None),
    aria_pressed: dataflow.literal(None),
    aria_read_only: dataflow.literal(None),
    aria_required: dataflow.literal(None),
    aria_role_description: dataflow.literal(None),
    aria_row_count: dataflow.literal(None),
    aria_row_index: dataflow.literal(None),
    aria_row_span: dataflow.literal(None),
    aria_selected: dataflow.literal(None),
    aria_set_size: dataflow.literal(None),
    aria_sort: dataflow.literal(None),
    aria_value_max: dataflow.literal(None),
    aria_value_min: dataflow.literal(None),
    aria_value_now: dataflow.literal(None),
    aria_value_text: dataflow.literal(None),
    id: dataflow.literal(None),
    aria_disabled: dataflow.literal(None),
    class_names: dataflow.literal(None),
    nonce: dataflow.literal(None),
    role: dataflow.literal(None),
    slot: dataflow.literal(None),
    tab_index: dataflow.literal(None),
    autofocus: dataflow.literal(None),
    style: dataflow.literal(styling_api.empty()),
    events: attr.events(),
    window_events: window_events(),
    on_add: None,
    required_extensions: dataflow.literal(None),
    system_language: dataflow.literal(None),
    height: dataflow.literal(None),
    preserve_aspect_ratio: dataflow.literal(None),
    view_box: dataflow.literal(None),
    width: dataflow.literal(None),
    x: dataflow.literal(None),
    y: dataflow.literal(None),
    content: fragment.EmptyInput,
  )
}

pub fn svg(attrs: SvgAttrs) -> fragment.Fragment {
  let SvgAttrs(
    aria_atomic,
    aria_auto_complete,
    aria_busy,
    aria_checked,
    aria_col_count,
    aria_col_index,
    aria_col_span,
    aria_current,
    aria_description,
    aria_expanded,
    aria_has_popup,
    aria_hidden,
    aria_invalid,
    aria_key_shortcuts,
    aria_label,
    aria_level,
    aria_live,
    aria_modal,
    aria_multi_line,
    aria_multi_selectable,
    aria_orientation,
    aria_placeholder,
    aria_pos_in_set,
    aria_pressed,
    aria_read_only,
    aria_required,
    aria_role_description,
    aria_row_count,
    aria_row_index,
    aria_row_span,
    aria_selected,
    aria_set_size,
    aria_sort,
    aria_value_max,
    aria_value_min,
    aria_value_now,
    aria_value_text,
    id,
    aria_disabled,
    class_names,
    nonce,
    role,
    slot,
    tab_index,
    autofocus,
    style,
    events,
    window_events,
    on_add,
    required_extensions,
    system_language,
    height,
    preserve_aspect_ratio,
    view_box,
    width,
    x,
    y,
    content,
  ) = attrs
  let attrs =
    attr.common_reactive_aria(
      attr.map_optional(aria_atomic, attr.bool_text),
      attr.map_optional(aria_auto_complete, aria_text.auto_complete),
      attr.map_optional(aria_busy, attr.bool_text),
      attr.map_optional(aria_checked, aria_text.bool_mixed_undefined),
      aria_col_count,
      aria_col_index,
      aria_col_span,
      attr.map_optional(aria_current, aria_text.current),
      aria_description,
      attr.map_optional(aria_expanded, aria_text.bool_undefined),
      attr.map_optional(aria_has_popup, aria_text.has_popup),
      attr.map_optional(aria_hidden, aria_text.bool_undefined),
      aria_invalid,
      aria_key_shortcuts,
      aria_label,
      aria_level,
      attr.map_optional(aria_live, aria_text.live),
      attr.map_optional(aria_modal, attr.bool_text),
      attr.map_optional(aria_multi_line, attr.bool_text),
      attr.map_optional(aria_multi_selectable, attr.bool_text),
      attr.map_optional(aria_orientation, aria_text.orientation),
      aria_placeholder,
      aria_pos_in_set,
      attr.map_optional(aria_pressed, aria_text.bool_mixed_undefined),
      attr.map_optional(aria_read_only, attr.bool_text),
      attr.map_optional(aria_required, attr.bool_text),
      aria_role_description,
      aria_row_count,
      aria_row_index,
      aria_row_span,
      attr.map_optional(aria_selected, aria_text.bool_undefined),
      aria_set_size,
      attr.map_optional(aria_sort, aria_text.sort),
      aria_value_max,
      aria_value_min,
      aria_value_now,
      aria_value_text,
      id,
      aria_disabled,
      class_names,
      nonce,
      role,
      slot,
      tab_index,
      autofocus,
      events,
    )
  let attrs = attr.add_style(attrs, style)
  let attrs = attr.add_events(attrs, events)
  let attrs = add_window_events(attrs, window_events)
  let attrs = attr.add_on_add(attrs, on_add)
  let attrs =
    attr.add_string_list_node(attrs, "requiredExtensions", required_extensions)
  let attrs =
    attr.add_string_list_node(attrs, "systemLanguage", system_language)
  let attrs =
    list.append(attrs, [
      fragment.ReactiveOptionalStringAttribute(
        "height",
        attr.map_optional(height, value_text),
      ),
    ])
  let attrs =
    list.append(attrs, [
      fragment.ReactiveOptionalStringAttribute(
        "preserveAspectRatio",
        attr.map_optional(
          preserve_aspect_ratio,
          data.svg_preserve_aspect_ratio_text,
        ),
      ),
    ])
  let attrs =
    list.append(attrs, [
      fragment.ReactiveOptionalStringAttribute(
        "viewBox",
        attr.map_optional(view_box, rect_text),
      ),
    ])
  let attrs =
    list.append(attrs, [
      fragment.ReactiveOptionalStringAttribute(
        "width",
        attr.map_optional(width, value_text),
      ),
    ])
  let attrs =
    list.append(attrs, [
      fragment.ReactiveOptionalStringAttribute(
        "x",
        attr.map_optional(x, value_text),
      ),
    ])
  let attrs =
    list.append(attrs, [
      fragment.ReactiveOptionalStringAttribute(
        "y",
        attr.map_optional(y, value_text),
      ),
    ])
  fragment.element_in(fragment.Svg, "svg", attrs, content)
}

fn value_text(value: data.SvgLengthPctOpt) -> String {
  case value {
    data.SvgPercentage(value) -> styling.percentage_text(value)
    data.SvgLengthValue(value) -> data.svg_length_text(value)
    data.SvgNumber(value) -> styling.number_text(value)
  }
}

// Portal attributes stay tag-specific at the call site, while the DOM node
// itself remains the opaque value supplied by the browser FFI.
pub fn portal(node: fragment.Node, attrs: GAttrs) -> fragment.Fragment {
  to_portal(node, g(attrs))
}

pub fn portal_rect(node: fragment.Node, attrs: RectAttrs) -> fragment.Fragment {
  to_portal(node, rect(attrs))
}

pub fn portal_svg(node: fragment.Node, attrs: SvgAttrs) -> fragment.Fragment {
  to_portal(node, svg(attrs))
}

fn to_portal(
  node: fragment.Node,
  value: fragment.Fragment,
) -> fragment.Fragment {
  case value {
    fragment.ElementNode(_, _, attributes, content) ->
      fragment.PortalNode(node, attributes, content)
    _ -> panic as "expected an element fragment"
  }
}

fn rect_text(value: data.Rect) -> String {
  let #(left, top, width, height) = value
  styling.number_text(left)
  <> " "
  <> styling.number_text(top)
  <> " "
  <> styling.number_text(width)
  <> " "
  <> styling.number_text(height)
}

fn add_window_events(
  attrs: List(fragment.Attribute),
  events: WindowEvents,
) -> List(fragment.Attribute) {
  let WindowEvents(
    on_after_print,
    on_before_print,
    on_before_unload,
    on_gamepad_connected,
    on_gamepad_disconnected,
    on_hash_change,
    on_language_change,
    on_message,
    on_message_error,
    on_offline,
    on_online,
    on_page_hide,
    on_page_show,
    on_pop_state,
    on_rejection_handled,
    on_storage,
    on_unhandled_rejection,
    on_unload,
  ) = events
  let attrs = attr.add_event(attrs, "afterprint", on_after_print)
  let attrs = attr.add_event(attrs, "beforeprint", on_before_print)
  let attrs = attr.add_event(attrs, "beforeunload", on_before_unload)
  let attrs = attr.add_event(attrs, "gamepadconnected", on_gamepad_connected)
  let attrs =
    attr.add_event(attrs, "gamepaddisconnected", on_gamepad_disconnected)
  let attrs = attr.add_event(attrs, "hashchange", on_hash_change)
  let attrs = attr.add_event(attrs, "languagechange", on_language_change)
  let attrs = attr.add_event(attrs, "message", on_message)
  let attrs = attr.add_event(attrs, "messageerror", on_message_error)
  let attrs = attr.add_event(attrs, "offline", on_offline)
  let attrs = attr.add_event(attrs, "online", on_online)
  let attrs = attr.add_event(attrs, "pagehide", on_page_hide)
  let attrs = attr.add_event(attrs, "pageshow", on_page_show)
  let attrs = attr.add_event(attrs, "popstate", on_pop_state)
  let attrs = attr.add_event(attrs, "rejectionhandled", on_rejection_handled)
  let attrs = attr.add_event(attrs, "storage", on_storage)
  let attrs =
    attr.add_event(attrs, "unhandledrejection", on_unhandled_rejection)
  attr.add_event(attrs, "unload", on_unload)
}
