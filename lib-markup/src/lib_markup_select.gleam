import gleam/int
import gleam/option.{type Option, None, Some}
import gleam/string
import lib_markup_data as data
import lib_markup_html as html
import lib_markup_math as math
import lib_styling_data as styling
import lib_styling_select as styling_select

// `select.attr` is the record form of the source's AllAttrs-derived selector
// input. Only attributes whose source kind is string or boolean are present;
// listeners, content, style, and onAdd cannot be CSS attribute selectors.
//
// The source receives an object and therefore observes its property insertion
// order. Gleam records have a declaration order, so this port uses the
// declaration order below as the selector's canonical order. Attribute
// selectors are order-independent in CSS, and the canonical order keeps the
// generated rule key stable across record updates.
pub type SelectAttrs {
  SelectAttrs(
    access_key: Option(String),
    alt_text: Option(String),
    aria_atomic: Option(Bool),
    aria_auto_complete: Option(html.AriaAutoComplete),
    aria_busy: Option(Bool),
    aria_checked: Option(html.AriaBoolMixedUndefined),
    aria_col_count: Option(String),
    aria_col_index: Option(String),
    aria_col_span: Option(String),
    aria_current: Option(html.AriaCurrent),
    aria_description: Option(String),
    aria_disabled: Option(Bool),
    aria_expanded: Option(html.AriaBoolUndefined),
    aria_has_popup: Option(html.AriaHasPopup),
    aria_hidden: Option(html.AriaBoolUndefined),
    aria_invalid: Option(String),
    aria_key_shortcuts: Option(String),
    aria_label: Option(String),
    aria_level: Option(String),
    aria_live: Option(html.AriaLive),
    aria_modal: Option(Bool),
    aria_multi_line: Option(Bool),
    aria_multi_selectable: Option(Bool),
    aria_orientation: Option(html.AriaOrientation),
    aria_placeholder: Option(String),
    aria_pos_in_set: Option(String),
    aria_pressed: Option(html.AriaBoolMixedUndefined),
    aria_read_only: Option(Bool),
    aria_required: Option(Bool),
    aria_role_description: Option(String),
    aria_row_count: Option(String),
    aria_row_index: Option(String),
    aria_row_span: Option(String),
    aria_selected: Option(html.AriaBoolUndefined),
    aria_set_size: Option(String),
    aria_sort: Option(html.AriaSort),
    aria_value_max: Option(String),
    aria_value_min: Option(String),
    aria_value_now: Option(String),
    aria_value_text: Option(String),
    autocapitalize: Option(html.Autocapitalize),
    autocomplete: Option(html.Autocomplete),
    autofocus: Option(Bool),
    checked: Option(Bool),
    class_names: Option(List(String)),
    content_editable: Option(html.ContentEditable),
    dir: Option(html.HtmlDir),
    disabled: Option(Bool),
    display: Option(Display),
    display_style: Option(Bool),
    draggable: Option(Bool),
    enter_key_hint: Option(html.EnterKeyHint),
    fence: Option(Bool),
    form: Option(String),
    form_action: Option(String),
    form_enctype: Option(html.FormEnctype),
    form_method: Option(html.FormMethod),
    form_no_validate: Option(Bool),
    form_target: Option(String),
    height: Option(data.SvgLengthPctOpt),
    hidden: Option(html.HiddenValue),
    id: Option(String),
    inert: Option(Bool),
    input_mode: Option(html.InputMode),
    l_space: Option(math.MathValue),
    lang: Option(String),
    large_op: Option(Bool),
    list: Option(String),
    math_dir: Option(math.MathDir),
    math_variant: Option(math.MathVariant),
    max_length: Option(Float),
    max_size: Option(math.MathValue),
    min_length: Option(Float),
    min_size: Option(math.MathValue),
    moveable_limits: Option(Bool),
    name: Option(String),
    nonce: Option(String),
    operator_form: Option(math.OperatorForm),
    path_length: Option(Float),
    pattern: Option(String),
    placeholder: Option(String),
    preserve_aspect_ratio: Option(data.SvgPreserveAspectRatioOpt),
    r_space: Option(math.MathValue),
    read_only: Option(Bool),
    required: Option(Bool),
    required_extensions: Option(List(String)),
    role: Option(String),
    rx: Option(data.SvgLengthPctOpt),
    ry: Option(data.SvgLengthPctOpt),
    script_level: Option(Float),
    separator: Option(Bool),
    size: Option(Float),
    slot: Option(String),
    spellcheck: Option(String),
    stretchy: Option(Bool),
    symmetric: Option(Bool),
    system_language: Option(List(String)),
    tab_index: Option(Float),
    title: Option(String),
    translate: Option(html.Translate),
    type_: Option(String),
    value: Option(String),
    view_box: Option(data.Rect),
    width: Option(data.SvgLengthPctOpt),
    x: Option(data.SvgLengthPctOpt),
    y: Option(data.SvgLengthPctOpt),
  )
}

pub type Display {
  DisplayBlock
  DisplayInline
}

pub type Tag {
  Button
  Div
  G
  H1
  H2
  H3
  H4
  H5
  H6
  Hr
  Input
  Math
  Mi
  Mo
  P
  Rect
  Span
  Svg
  Title
}

pub fn attrs() -> SelectAttrs {
  SelectAttrs(
    access_key: None,
    alt_text: None,
    aria_atomic: None,
    aria_auto_complete: None,
    aria_busy: None,
    aria_checked: None,
    aria_col_count: None,
    aria_col_index: None,
    aria_col_span: None,
    aria_current: None,
    aria_description: None,
    aria_disabled: None,
    aria_expanded: None,
    aria_has_popup: None,
    aria_hidden: None,
    aria_invalid: None,
    aria_key_shortcuts: None,
    aria_label: None,
    aria_level: None,
    aria_live: None,
    aria_modal: None,
    aria_multi_line: None,
    aria_multi_selectable: None,
    aria_orientation: None,
    aria_placeholder: None,
    aria_pos_in_set: None,
    aria_pressed: None,
    aria_read_only: None,
    aria_required: None,
    aria_role_description: None,
    aria_row_count: None,
    aria_row_index: None,
    aria_row_span: None,
    aria_selected: None,
    aria_set_size: None,
    aria_sort: None,
    aria_value_max: None,
    aria_value_min: None,
    aria_value_now: None,
    aria_value_text: None,
    autocapitalize: None,
    autocomplete: None,
    autofocus: None,
    checked: None,
    class_names: None,
    content_editable: None,
    dir: None,
    disabled: None,
    display: None,
    display_style: None,
    draggable: None,
    enter_key_hint: None,
    fence: None,
    form: None,
    form_action: None,
    form_enctype: None,
    form_method: None,
    form_no_validate: None,
    form_target: None,
    height: None,
    hidden: None,
    id: None,
    inert: None,
    input_mode: None,
    l_space: None,
    lang: None,
    large_op: None,
    list: None,
    math_dir: None,
    math_variant: None,
    max_length: None,
    max_size: None,
    min_length: None,
    min_size: None,
    moveable_limits: None,
    name: None,
    nonce: None,
    operator_form: None,
    path_length: None,
    pattern: None,
    placeholder: None,
    preserve_aspect_ratio: None,
    r_space: None,
    read_only: None,
    required: None,
    required_extensions: None,
    role: None,
    rx: None,
    ry: None,
    script_level: None,
    separator: None,
    size: None,
    slot: None,
    spellcheck: None,
    stretchy: None,
    symmetric: None,
    system_language: None,
    tab_index: None,
    title: None,
    translate: None,
    type_: None,
    value: None,
    view_box: None,
    width: None,
    x: None,
    y: None,
  )
}

pub fn attr(value: SelectAttrs) -> styling_select.SelectTerm {
  let SelectAttrs(
    access_key,
    alt_text,
    aria_atomic,
    aria_auto_complete,
    aria_busy,
    aria_checked,
    aria_col_count,
    aria_col_index,
    aria_col_span,
    aria_current,
    aria_description,
    aria_disabled,
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
    autocapitalize,
    autocomplete,
    autofocus,
    checked,
    class_names,
    content_editable,
    dir,
    disabled,
    display,
    display_style,
    draggable,
    enter_key_hint,
    fence,
    form,
    form_action,
    form_enctype,
    form_method,
    form_no_validate,
    form_target,
    height,
    hidden,
    id,
    inert,
    input_mode,
    l_space,
    lang,
    large_op,
    list,
    math_dir,
    math_variant,
    max_length,
    max_size,
    min_length,
    min_size,
    moveable_limits,
    name,
    nonce,
    operator_form,
    path_length,
    pattern,
    placeholder,
    preserve_aspect_ratio,
    r_space,
    read_only,
    required,
    required_extensions,
    role,
    rx,
    ry,
    script_level,
    separator,
    size,
    slot,
    spellcheck,
    stretchy,
    symmetric,
    system_language,
    tab_index,
    title,
    translate,
    type_,
    value,
    view_box,
    width,
    x,
    y,
  ) = value
  let output = add_string("", "accesskey", access_key)
  let output = add_string(output, "alttext", alt_text)
  let output = add_bool_string(output, "aria-atomic", aria_atomic)
  let output =
    add_string(
      output,
      "aria-autocomplete",
      option.map(aria_auto_complete, aria_auto_complete_text),
    )
  let output = add_bool_string(output, "aria-busy", aria_busy)
  let output =
    add_string(
      output,
      "aria-checked",
      option.map(aria_checked, aria_bool_mixed_undefined_text),
    )
  let output = add_string(output, "aria-colcount", aria_col_count)
  let output = add_string(output, "aria-colindex", aria_col_index)
  let output = add_string(output, "aria-colspan", aria_col_span)
  let output =
    add_string(
      output,
      "aria-current",
      option.map(aria_current, aria_current_text),
    )
  let output = add_string(output, "aria-description", aria_description)
  let output = add_bool_string(output, "aria-disabled", aria_disabled)
  let output =
    add_string(
      output,
      "aria-expanded",
      option.map(aria_expanded, aria_bool_undefined_text),
    )
  let output =
    add_string(
      output,
      "aria-haspopup",
      option.map(aria_has_popup, aria_has_popup_text),
    )
  let output =
    add_string(
      output,
      "aria-hidden",
      option.map(aria_hidden, aria_bool_undefined_text),
    )
  let output = add_string(output, "aria-invalid", aria_invalid)
  let output = add_string(output, "aria-keyshortcuts", aria_key_shortcuts)
  let output = add_string(output, "aria-label", aria_label)
  let output = add_string(output, "aria-level", aria_level)
  let output =
    add_string(output, "aria-live", option.map(aria_live, aria_live_text))
  let output = add_bool_string(output, "aria-modal", aria_modal)
  let output = add_bool_string(output, "aria-multiline", aria_multi_line)
  let output =
    add_bool_string(output, "aria-multiselectable", aria_multi_selectable)
  let output =
    add_string(
      output,
      "aria-orientation",
      option.map(aria_orientation, aria_orientation_text),
    )
  let output = add_string(output, "aria-placeholder", aria_placeholder)
  let output = add_string(output, "aria-posinset", aria_pos_in_set)
  let output =
    add_string(
      output,
      "aria-pressed",
      option.map(aria_pressed, aria_bool_mixed_undefined_text),
    )
  let output = add_bool_string(output, "aria-readonly", aria_read_only)
  let output = add_bool_string(output, "aria-required", aria_required)
  let output = add_string(output, "aria-roledescription", aria_role_description)
  let output = add_string(output, "aria-rowcount", aria_row_count)
  let output = add_string(output, "aria-rowindex", aria_row_index)
  let output = add_string(output, "aria-rowspan", aria_row_span)
  let output =
    add_string(
      output,
      "aria-selected",
      option.map(aria_selected, aria_bool_undefined_text),
    )
  let output = add_string(output, "aria-setsize", aria_set_size)
  let output =
    add_string(output, "aria-sort", option.map(aria_sort, aria_sort_text))
  let output = add_string(output, "aria-valuemax", aria_value_max)
  let output = add_string(output, "aria-valuemin", aria_value_min)
  let output = add_string(output, "aria-valuenow", aria_value_now)
  let output = add_string(output, "aria-valuetext", aria_value_text)
  let output =
    add_string(
      output,
      "autocapitalize",
      option.map(autocapitalize, autocapitalize_text),
    )
  let output =
    add_string(
      output,
      "autocomplete",
      option.map(autocomplete, autocomplete_text),
    )
  let output = add_bool(output, "autofocus", autofocus)
  let output = add_bool(output, "checked", checked)
  let output = add_list(output, "class", class_names)
  let output =
    add_string(
      output,
      "contenteditable",
      option.map(content_editable, content_editable_text),
    )
  let output = add_string(output, "dir", option.map(dir, dir_text))
  let output = add_bool(output, "disabled", disabled)
  let output = add_string(output, "display", option.map(display, display_text))
  let output = add_bool_string(output, "displaystyle", display_style)
  let output = add_bool_string(output, "draggable", draggable)
  let output =
    add_string(
      output,
      "enterkeyhint",
      option.map(enter_key_hint, enter_key_hint_text),
    )
  let output = add_bool_string(output, "fence", fence)
  let output = add_string(output, "form", form)
  let output = add_string(output, "formaction", form_action)
  let output =
    add_string(
      output,
      "formenctype",
      option.map(form_enctype, form_enctype_text),
    )
  let output =
    add_string(output, "formmethod", option.map(form_method, form_method_text))
  let output = add_bool(output, "formnovalidate", form_no_validate)
  let output = add_string(output, "formtarget", form_target)
  let output =
    add_string(output, "height", option.map(height, svg_length_pct_text))
  let output = add_hidden(output, hidden)
  let output = add_string(output, "id", id)
  let output = add_bool(output, "inert", inert)
  let output =
    add_string(output, "inputmode", option.map(input_mode, input_mode_text))
  let output = add_string(output, "lspace", option.map(l_space, value_text))
  let output = add_string(output, "lang", lang)
  let output = add_bool_string(output, "largeop", large_op)
  let output = add_string(output, "list", list)
  let output = add_string(output, "dir", option.map(math_dir, math_dir_text))
  let output =
    add_string(
      output,
      "mathvariant",
      option.map(math_variant, math_variant_text),
    )
  let output = add_number(output, "maxlength", max_length)
  let output = add_string(output, "maxsize", option.map(max_size, value_text))
  let output = add_number(output, "minlength", min_length)
  let output = add_string(output, "minsize", option.map(min_size, value_text))
  let output = add_bool_string(output, "moveablelimits", moveable_limits)
  let output = add_string(output, "name", name)
  let output = add_string(output, "nonce", nonce)
  let output =
    add_string(output, "form", option.map(operator_form, operator_form_text))
  let output = add_number(output, "pathLength", path_length)
  let output = add_string(output, "pattern", pattern)
  let output = add_string(output, "placeholder", placeholder)
  let output =
    add_string(
      output,
      "preserveAspectRatio",
      option.map(preserve_aspect_ratio, data.svg_preserve_aspect_ratio_text),
    )
  let output = add_string(output, "rspace", option.map(r_space, value_text))
  let output = add_bool(output, "readonly", read_only)
  let output = add_bool(output, "required", required)
  let output = add_list(output, "requiredExtensions", required_extensions)
  let output = add_string(output, "role", role)
  let output = add_string(output, "rx", option.map(rx, svg_length_pct_text))
  let output = add_string(output, "ry", option.map(ry, svg_length_pct_text))
  let output = add_number(output, "scriptlevel", script_level)
  let output = add_bool_string(output, "separator", separator)
  let output = add_number(output, "size", size)
  let output = add_string(output, "slot", slot)
  let output = add_string(output, "spellcheck", spellcheck)
  let output = add_bool_string(output, "stretchy", stretchy)
  let output = add_bool_string(output, "symmetric", symmetric)
  let output = add_list(output, "systemLanguage", system_language)
  let output = add_number(output, "tabindex", tab_index)
  let output = add_string(output, "title", title)
  let output =
    add_string(output, "translate", option.map(translate, translate_text))
  let output = add_string(output, "type", type_)
  let output = add_string(output, "value", value)
  let output = add_string(output, "viewBox", option.map(view_box, rect_text))
  let output =
    add_string(output, "width", option.map(width, svg_length_pct_text))
  let output = add_string(output, "x", option.map(x, svg_length_pct_text))
  styling_select.raw(add_string(output, "y", option.map(y, svg_length_pct_text)))
}

pub fn type_(first: Tag, rest: List(Tag)) -> styling_select.SelectTerm {
  styling_select.raw(":where(" <> join_tags([first, ..rest]) <> ")")
}

fn add_string(output: String, name: String, value: Option(String)) -> String {
  case value {
    None -> output
    Some(value) -> output <> "[" <> name <> "=" <> json_string(value) <> "]"
  }
}

fn add_bool_string(
  output: String,
  name: String,
  value: Option(Bool),
) -> String {
  add_string(output, name, option.map(value, bool_text))
}

fn add_bool(output: String, name: String, value: Option(Bool)) -> String {
  case value {
    None -> output
    Some(True) -> output <> "[" <> name <> "]"
    Some(False) -> output <> ":not([" <> name <> "])"
  }
}

fn add_hidden(output: String, value: Option(html.HiddenValue)) -> String {
  case value {
    None -> output
    Some(html.HiddenBoolean) -> add_bool(output, "hidden", Some(True))
    Some(html.HiddenUntilFound) ->
      add_string(output, "hidden", Some("until-found"))
  }
}

fn add_list(
  output: String,
  name: String,
  value: Option(List(String)),
) -> String {
  add_string(output, name, option.map(value, join_words))
}

fn add_number(output: String, name: String, value: Option(Float)) -> String {
  add_string(output, name, option.map(value, styling.number_text))
}

fn bool_text(value: Bool) -> String {
  case value {
    True -> "true"
    False -> "false"
  }
}

fn display_text(value: Display) -> String {
  case value {
    DisplayBlock -> "block"
    DisplayInline -> "inline"
  }
}

fn aria_auto_complete_text(value: html.AriaAutoComplete) -> String {
  case value {
    html.AriaAutoCompleteBoth -> "both"
    html.AriaAutoCompleteInline -> "inline"
    html.AriaAutoCompleteList -> "list"
    html.AriaAutoCompleteNone -> "none"
  }
}

fn aria_bool_undefined_text(value: html.AriaBoolUndefined) -> String {
  case value {
    html.AriaBool(value) -> bool_text(value)
    html.AriaUndefined -> "undefined"
  }
}

fn aria_bool_mixed_undefined_text(
  value: html.AriaBoolMixedUndefined,
) -> String {
  case value {
    html.AriaBoolMixed(value) -> bool_text(value)
    html.AriaMixed -> "mixed"
    html.AriaMixedUndefined -> "undefined"
  }
}

fn aria_current_text(value: html.AriaCurrent) -> String {
  case value {
    html.AriaCurrentBool(value) -> bool_text(value)
    html.AriaCurrentDate -> "date"
    html.AriaCurrentLocation -> "location"
    html.AriaCurrentPage -> "page"
    html.AriaCurrentStep -> "step"
    html.AriaCurrentTime -> "time"
  }
}

fn aria_has_popup_text(value: html.AriaHasPopup) -> String {
  case value {
    html.AriaHasPopupBool(value) -> bool_text(value)
    html.AriaHasPopupDialog -> "dialog"
    html.AriaHasPopupGrid -> "grid"
    html.AriaHasPopupListbox -> "listbox"
    html.AriaHasPopupMenu -> "menu"
    html.AriaHasPopupTree -> "tree"
  }
}

fn aria_live_text(value: html.AriaLive) -> String {
  case value {
    html.AriaLiveAssertive -> "assertive"
    html.AriaLiveOff -> "off"
    html.AriaLivePolite -> "polite"
  }
}

fn aria_orientation_text(value: html.AriaOrientation) -> String {
  case value {
    html.AriaOrientationHorizontal -> "horizontal"
    html.AriaOrientationUndefined -> "undefined"
    html.AriaOrientationVertical -> "vertical"
  }
}

fn aria_sort_text(value: html.AriaSort) -> String {
  case value {
    html.AriaSortAscending -> "ascending"
    html.AriaSortDescending -> "descending"
    html.AriaSortNone -> "none"
    html.AriaSortOther -> "other"
  }
}

fn autocapitalize_text(value: html.Autocapitalize) -> String {
  case value {
    html.AutocapitalizeCharacters -> "characters"
    html.AutocapitalizeNone -> "none"
    html.AutocapitalizeSentences -> "sentences"
    html.AutocapitalizeWords -> "words"
  }
}

fn autocomplete_text(value: html.Autocomplete) -> String {
  case value {
    html.AutocompleteOff -> "off"
    html.AutocompleteOn -> "on"
    html.AutocompleteTokens(values) -> autocomplete_tokens_text(values)
  }
}

fn autocomplete_tokens_text(values: List(html.AutocompleteToken)) -> String {
  case values {
    [] -> ""
    [first, ..rest] ->
      case rest {
        [] -> autocomplete_token_text(first)
        _ ->
          autocomplete_token_text(first)
          <> " "
          <> autocomplete_tokens_text(rest)
      }
  }
}

fn autocomplete_token_text(value: html.AutocompleteToken) -> String {
  case value {
    html.AutocompleteEmail -> "email"
    html.AutocompleteName -> "name"
    html.AutocompleteSection(value) -> "section-" <> value
  }
}

fn content_editable_text(value: html.ContentEditable) -> String {
  case value {
    html.ContentEditableValue(value) -> bool_text(value)
    html.ContentEditablePlaintextOnly -> "plaintext-only"
  }
}

fn dir_text(value: html.HtmlDir) -> String {
  case value {
    html.HtmlDirAuto -> "auto"
    html.HtmlDirLtr -> "ltr"
    html.HtmlDirRtl -> "rtl"
  }
}

fn enter_key_hint_text(value: html.EnterKeyHint) -> String {
  case value {
    html.EnterKeyDone -> "done"
    html.EnterKeyEnter -> "enter"
    html.EnterKeyGo -> "go"
    html.EnterKeyNext -> "next"
    html.EnterKeyPrevious -> "previous"
    html.EnterKeySearch -> "search"
    html.EnterKeySend -> "send"
  }
}

fn form_enctype_text(value: html.FormEnctype) -> String {
  case value {
    html.FormUrlEncoded -> "application/x-www-form-urlencoded"
    html.FormMultipart -> "multipart/form-data"
    html.FormTextPlain -> "text/plain"
  }
}

fn form_method_text(value: html.FormMethod) -> String {
  case value {
    html.FormDialog -> "dialog"
    html.FormGet -> "get"
    html.FormPost -> "post"
  }
}

fn input_mode_text(value: html.InputMode) -> String {
  case value {
    html.InputModeDecimal -> "decimal"
    html.InputModeEmail -> "email"
    html.InputModeNone -> "none"
    html.InputModeNumeric -> "numeric"
    html.InputModeSearch -> "search"
    html.InputModeTel -> "tel"
    html.InputModeText -> "text"
    html.InputModeUrl -> "url"
  }
}

fn translate_text(value: html.Translate) -> String {
  case value {
    html.TranslateNo -> "no"
    html.TranslateYes -> "yes"
  }
}

fn join_words(values: List(String)) -> String {
  case values {
    [] -> ""
    [first, ..rest] -> join_words_after(rest, first)
  }
}

fn join_words_after(values: List(String), output: String) -> String {
  case values {
    [] -> output
    [first, ..rest] -> join_words_after(rest, output <> " " <> first)
  }
}

fn value_text(value: math.MathValue) -> String {
  case value {
    math.MathLength(value) -> styling.length_text(value)
    math.MathPercentage(value) -> styling.percentage_text(value)
  }
}

fn math_dir_text(value: math.MathDir) -> String {
  case value {
    math.MathLtr -> "ltr"
    math.MathRtl -> "rtl"
  }
}

fn math_variant_text(value: math.MathVariant) -> String {
  case value {
    math.MathNormal -> "normal"
  }
}

fn operator_form_text(value: math.OperatorForm) -> String {
  case value {
    math.FormInfix -> "infix"
    math.FormPostfix -> "postfix"
    math.FormPrefix -> "prefix"
  }
}

fn svg_length_pct_text(value: data.SvgLengthPctOpt) -> String {
  case value {
    data.SvgPercentage(value) -> styling.percentage_text(value)
    data.SvgLengthValue(value) -> data.svg_length_text(value)
    data.SvgNumber(value) -> styling.number_text(value)
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

fn tag_text(value: Tag) -> String {
  case value {
    Button -> "button"
    Div -> "div"
    G -> "g"
    H1 -> "h1"
    H2 -> "h2"
    H3 -> "h3"
    H4 -> "h4"
    H5 -> "h5"
    H6 -> "h6"
    Hr -> "hr"
    Input -> "input"
    Math -> "math"
    Mi -> "mi"
    Mo -> "mo"
    P -> "p"
    Rect -> "rect"
    Span -> "span"
    Svg -> "svg"
    Title -> "title"
  }
}

fn join_tags(values: List(Tag)) -> String {
  case values {
    [] -> ""
    [first] -> tag_text(first)
    [first, ..rest] -> tag_text(first) <> ", " <> join_tags(rest)
  }
}

fn json_string(value: String) -> String {
  "\"" <> escape_json(string.to_utf_codepoints(value)) <> "\""
}

fn escape_json(values) -> String {
  case values {
    [] -> ""
    [first, ..rest] -> escape_codepoint(first) <> escape_json(rest)
  }
}

fn escape_codepoint(value) -> String {
  let codepoint = string.utf_codepoint_to_int(value)
  case codepoint {
    8 -> "\\b"
    9 -> "\\t"
    10 -> "\\n"
    12 -> "\\f"
    13 -> "\\r"
    34 -> "\\\""
    92 -> "\\\\"
    value if value < 32 -> "\\u" <> padded_hex(value)
    _ -> string.from_utf_codepoints([value])
  }
}

fn padded_hex(value: Int) -> String {
  let hex = int.to_base16(value)
  case string.length(hex) {
    1 -> "000" <> hex
    2 -> "00" <> hex
    3 -> "0" <> hex
    _ -> hex
  }
}
