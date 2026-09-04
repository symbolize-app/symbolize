import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import lib_dataflow as dataflow
import lib_markup_attributes as markup_attributes
import lib_markup_fragment as fragment
import lib_styling as styling

pub type Autocapitalize {
  AutocapitalizeCharacters
  AutocapitalizeNone
  AutocapitalizeSentences
  AutocapitalizeWords
}

pub type ContentEditable {
  ContentEditableValue(Bool)
  ContentEditablePlaintextOnly
}

pub type HtmlDir {
  HtmlDirAuto
  HtmlDirLtr
  HtmlDirRtl
}

pub type EnterKeyHint {
  EnterKeyDone
  EnterKeyEnter
  EnterKeyGo
  EnterKeyNext
  EnterKeyPrevious
  EnterKeySearch
  EnterKeySend
}

pub type HiddenValue {
  HiddenBoolean
  HiddenUntilFound
}

pub type FormEnctype {
  FormUrlEncoded
  FormMultipart
  FormTextPlain
}

pub type FormMethod {
  FormDialog
  FormGet
  FormPost
}

pub type InputMode {
  InputModeDecimal
  InputModeEmail
  InputModeNone
  InputModeNumeric
  InputModeSearch
  InputModeTel
  InputModeText
  InputModeUrl
}

pub type Translate {
  TranslateNo
  TranslateYes
}

pub type AriaAutoComplete {
  AriaAutoCompleteBoth
  AriaAutoCompleteInline
  AriaAutoCompleteList
  AriaAutoCompleteNone
}

pub type AriaBoolUndefined {
  AriaBool(Bool)
  AriaUndefined
}

pub type AriaBoolMixedUndefined {
  AriaBoolMixed(Bool)
  AriaMixed
  AriaMixedUndefined
}

pub type AriaCurrent {
  AriaCurrentBool(Bool)
  AriaCurrentDate
  AriaCurrentLocation
  AriaCurrentPage
  AriaCurrentStep
  AriaCurrentTime
}

pub type AriaHasPopup {
  AriaHasPopupBool(Bool)
  AriaHasPopupDialog
  AriaHasPopupGrid
  AriaHasPopupListbox
  AriaHasPopupMenu
  AriaHasPopupTree
}

pub type AriaLive {
  AriaLiveAssertive
  AriaLiveOff
  AriaLivePolite
}

pub type AriaOrientation {
  AriaOrientationHorizontal
  AriaOrientationUndefined
  AriaOrientationVertical
}

pub type AriaSort {
  AriaSortAscending
  AriaSortDescending
  AriaSortNone
  AriaSortOther
}

pub type DivAttrs {
  DivAttrs(
    access_key: dataflow.NodeOpt(Option(String)),
    autocapitalize: dataflow.NodeOpt(Option(Autocapitalize)),
    content_editable: dataflow.NodeOpt(Option(ContentEditable)),
    dir: dataflow.NodeOpt(Option(HtmlDir)),
    draggable: dataflow.NodeOpt(Option(Bool)),
    enter_key_hint: dataflow.NodeOpt(Option(EnterKeyHint)),
    hidden: dataflow.NodeOpt(Option(HiddenValue)),
    inert: dataflow.NodeOpt(Option(Bool)),
    input_mode: dataflow.NodeOpt(Option(InputMode)),
    lang: dataflow.NodeOpt(Option(String)),
    aria_atomic: dataflow.NodeOpt(Option(Bool)),
    aria_auto_complete: dataflow.NodeOpt(Option(AriaAutoComplete)),
    aria_busy: dataflow.NodeOpt(Option(Bool)),
    aria_checked: dataflow.NodeOpt(Option(AriaBoolMixedUndefined)),
    aria_col_count: dataflow.NodeOpt(Option(String)),
    aria_col_index: dataflow.NodeOpt(Option(String)),
    aria_col_span: dataflow.NodeOpt(Option(String)),
    aria_current: dataflow.NodeOpt(Option(AriaCurrent)),
    aria_description: dataflow.NodeOpt(Option(String)),
    aria_expanded: dataflow.NodeOpt(Option(AriaBoolUndefined)),
    aria_has_popup: dataflow.NodeOpt(Option(AriaHasPopup)),
    aria_hidden: dataflow.NodeOpt(Option(AriaBoolUndefined)),
    aria_invalid: dataflow.NodeOpt(Option(String)),
    aria_key_shortcuts: dataflow.NodeOpt(Option(String)),
    aria_label: dataflow.NodeOpt(Option(String)),
    aria_level: dataflow.NodeOpt(Option(String)),
    aria_live: dataflow.NodeOpt(Option(AriaLive)),
    aria_modal: dataflow.NodeOpt(Option(Bool)),
    aria_multi_line: dataflow.NodeOpt(Option(Bool)),
    aria_multi_selectable: dataflow.NodeOpt(Option(Bool)),
    aria_orientation: dataflow.NodeOpt(Option(AriaOrientation)),
    aria_placeholder: dataflow.NodeOpt(Option(String)),
    aria_pos_in_set: dataflow.NodeOpt(Option(String)),
    aria_pressed: dataflow.NodeOpt(Option(AriaBoolMixedUndefined)),
    aria_read_only: dataflow.NodeOpt(Option(Bool)),
    aria_required: dataflow.NodeOpt(Option(Bool)),
    aria_role_description: dataflow.NodeOpt(Option(String)),
    aria_row_count: dataflow.NodeOpt(Option(String)),
    aria_row_index: dataflow.NodeOpt(Option(String)),
    aria_row_span: dataflow.NodeOpt(Option(String)),
    aria_selected: dataflow.NodeOpt(Option(AriaBoolUndefined)),
    aria_set_size: dataflow.NodeOpt(Option(String)),
    aria_sort: dataflow.NodeOpt(Option(AriaSort)),
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
    translate: dataflow.NodeOpt(Option(Translate)),
    style: dataflow.NodeOpt(styling.AtomOpt),
    events: markup_attributes.Events,
    on_add: Option(fragment.OnAdd),
    content: fragment.FragmentInput,
  )
}

pub fn div_attrs() -> DivAttrs {
  DivAttrs(
    access_key: dataflow.literal(None),
    autocapitalize: dataflow.literal(None),
    content_editable: dataflow.literal(None),
    dir: dataflow.literal(None),
    draggable: dataflow.literal(None),
    enter_key_hint: dataflow.literal(None),
    hidden: dataflow.literal(None),
    inert: dataflow.literal(None),
    input_mode: dataflow.literal(None),
    lang: dataflow.literal(None),
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
    title: dataflow.literal(None),
    nonce: dataflow.literal(None),
    role: dataflow.literal(None),
    slot: dataflow.literal(None),
    spellcheck: dataflow.literal(None),
    tab_index: dataflow.literal(None),
    autofocus: dataflow.literal(None),
    translate: dataflow.literal(None),
    style: dataflow.literal(styling.empty()),
    events: markup_attributes.events(),
    on_add: None,
    content: fragment.EmptyInput,
  )
}

pub fn div(attrs: DivAttrs) -> fragment.Fragment {
  common_element("div", attrs)
}

pub fn h1(attrs: DivAttrs) -> fragment.Fragment {
  common_element("h1", attrs)
}

pub fn h2(attrs: DivAttrs) -> fragment.Fragment {
  common_element("h2", attrs)
}

pub fn h3(attrs: DivAttrs) -> fragment.Fragment {
  common_element("h3", attrs)
}

pub fn h4(attrs: DivAttrs) -> fragment.Fragment {
  common_element("h4", attrs)
}

pub fn h5(attrs: DivAttrs) -> fragment.Fragment {
  common_element("h5", attrs)
}

pub fn h6(attrs: DivAttrs) -> fragment.Fragment {
  common_element("h6", attrs)
}

pub fn hr(attrs: DivAttrs) -> fragment.Fragment {
  common_element("hr", attrs)
}

pub fn p(attrs: DivAttrs) -> fragment.Fragment {
  common_element("p", attrs)
}

pub fn span(attrs: DivAttrs) -> fragment.Fragment {
  common_element("span", attrs)
}

pub fn title(attrs: DivAttrs) -> fragment.Fragment {
  common_element("title", attrs)
}

// The source portal API derives its attribute type from the existing element.
// Common HTML elements share the same record in this port, so this entry point
// keeps that source contract without exposing the fragment attribute list.
pub fn portal(node: fragment.Node, attrs: DivAttrs) -> fragment.Fragment {
  to_portal(node, common_element("div", attrs))
}

fn common_element(tag: String, attrs: DivAttrs) -> fragment.Fragment {
  let DivAttrs(
    access_key,
    autocapitalize,
    content_editable,
    dir,
    draggable,
    enter_key_hint,
    hidden,
    inert,
    input_mode,
    lang,
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
    title,
    nonce,
    role,
    slot,
    spellcheck,
    tab_index,
    autofocus,
    translate,
    style,
    events,
    on_add,
    content,
  ) = attrs
  let attributes =
    common_html(
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
      access_key,
      autocapitalize,
      content_editable,
      dir,
      draggable,
      enter_key_hint,
      hidden,
      inert,
      input_mode,
      lang,
      spellcheck,
      translate,
      id,
      aria_disabled,
      class_names,
      title,
      nonce,
      role,
      slot,
      tab_index,
      autofocus,
      events,
    )
  let attributes = markup_attributes.add_style(attributes, style)
  let attributes = markup_attributes.add_on_add(attributes, on_add)
  fragment.element(tag, attributes, content)
}

fn aria_auto_complete_text(value: AriaAutoComplete) -> String {
  case value {
    AriaAutoCompleteBoth -> "both"
    AriaAutoCompleteInline -> "inline"
    AriaAutoCompleteList -> "list"
    AriaAutoCompleteNone -> "none"
  }
}

fn aria_bool_undefined_text(value: AriaBoolUndefined) -> String {
  case value {
    AriaBool(value) -> markup_attributes.bool_text(value)
    AriaUndefined -> "undefined"
  }
}

fn aria_bool_mixed_undefined_text(value: AriaBoolMixedUndefined) -> String {
  case value {
    AriaBoolMixed(value) -> markup_attributes.bool_text(value)
    AriaMixed -> "mixed"
    AriaMixedUndefined -> "undefined"
  }
}

fn aria_current_text(value: AriaCurrent) -> String {
  case value {
    AriaCurrentBool(value) -> markup_attributes.bool_text(value)
    AriaCurrentDate -> "date"
    AriaCurrentLocation -> "location"
    AriaCurrentPage -> "page"
    AriaCurrentStep -> "step"
    AriaCurrentTime -> "time"
  }
}

fn aria_has_popup_text(value: AriaHasPopup) -> String {
  case value {
    AriaHasPopupBool(value) -> markup_attributes.bool_text(value)
    AriaHasPopupDialog -> "dialog"
    AriaHasPopupGrid -> "grid"
    AriaHasPopupListbox -> "listbox"
    AriaHasPopupMenu -> "menu"
    AriaHasPopupTree -> "tree"
  }
}

fn autocapitalize_text(value: Autocapitalize) -> String {
  case value {
    AutocapitalizeCharacters -> "characters"
    AutocapitalizeNone -> "none"
    AutocapitalizeSentences -> "sentences"
    AutocapitalizeWords -> "words"
  }
}

fn aria_live_text(value: AriaLive) -> String {
  case value {
    AriaLiveAssertive -> "assertive"
    AriaLiveOff -> "off"
    AriaLivePolite -> "polite"
  }
}

fn aria_orientation_text(value: AriaOrientation) -> String {
  case value {
    AriaOrientationHorizontal -> "horizontal"
    AriaOrientationUndefined -> "undefined"
    AriaOrientationVertical -> "vertical"
  }
}

fn aria_sort_text(value: AriaSort) -> String {
  case value {
    AriaSortAscending -> "ascending"
    AriaSortDescending -> "descending"
    AriaSortNone -> "none"
    AriaSortOther -> "other"
  }
}

fn content_editable_text(value: ContentEditable) -> String {
  case value {
    ContentEditableValue(value) -> markup_attributes.bool_text(value)
    ContentEditablePlaintextOnly -> "plaintext-only"
  }
}

fn dir_text(value: HtmlDir) -> String {
  case value {
    HtmlDirAuto -> "auto"
    HtmlDirLtr -> "ltr"
    HtmlDirRtl -> "rtl"
  }
}

fn enter_key_hint_text(value: EnterKeyHint) -> String {
  case value {
    EnterKeyDone -> "done"
    EnterKeyEnter -> "enter"
    EnterKeyGo -> "go"
    EnterKeyNext -> "next"
    EnterKeyPrevious -> "previous"
    EnterKeySearch -> "search"
    EnterKeySend -> "send"
  }
}

fn hidden_text(value: HiddenValue) -> String {
  case value {
    HiddenBoolean -> ""
    HiddenUntilFound -> "until-found"
  }
}

fn input_mode_text(value: InputMode) -> String {
  case value {
    InputModeDecimal -> "decimal"
    InputModeEmail -> "email"
    InputModeNone -> "none"
    InputModeNumeric -> "numeric"
    InputModeSearch -> "search"
    InputModeTel -> "tel"
    InputModeText -> "text"
    InputModeUrl -> "url"
  }
}

fn translate_text(value: Translate) -> String {
  case value {
    TranslateNo -> "no"
    TranslateYes -> "yes"
  }
}

pub type ButtonAttrs {
  SubmitButton(SubmitButtonAttrs)
  PlainButton(PlainButtonAttrs)
  ResetButton(ResetButtonAttrs)
}

pub type SubmitButtonAttrs {
  SubmitButtonAttrs(
    access_key: dataflow.NodeOpt(Option(String)),
    autocapitalize: dataflow.NodeOpt(Option(Autocapitalize)),
    content_editable: dataflow.NodeOpt(Option(ContentEditable)),
    dir: dataflow.NodeOpt(Option(HtmlDir)),
    draggable: dataflow.NodeOpt(Option(Bool)),
    enter_key_hint: dataflow.NodeOpt(Option(EnterKeyHint)),
    hidden: dataflow.NodeOpt(Option(HiddenValue)),
    inert: dataflow.NodeOpt(Option(Bool)),
    input_mode: dataflow.NodeOpt(Option(InputMode)),
    lang: dataflow.NodeOpt(Option(String)),
    spellcheck: dataflow.NodeOpt(Option(String)),
    translate: dataflow.NodeOpt(Option(Translate)),
    aria_atomic: dataflow.NodeOpt(Option(Bool)),
    aria_auto_complete: dataflow.NodeOpt(Option(AriaAutoComplete)),
    aria_busy: dataflow.NodeOpt(Option(Bool)),
    aria_checked: dataflow.NodeOpt(Option(AriaBoolMixedUndefined)),
    aria_col_count: dataflow.NodeOpt(Option(String)),
    aria_col_index: dataflow.NodeOpt(Option(String)),
    aria_col_span: dataflow.NodeOpt(Option(String)),
    aria_current: dataflow.NodeOpt(Option(AriaCurrent)),
    aria_description: dataflow.NodeOpt(Option(String)),
    aria_expanded: dataflow.NodeOpt(Option(AriaBoolUndefined)),
    aria_has_popup: dataflow.NodeOpt(Option(AriaHasPopup)),
    aria_hidden: dataflow.NodeOpt(Option(AriaBoolUndefined)),
    aria_invalid: dataflow.NodeOpt(Option(String)),
    aria_key_shortcuts: dataflow.NodeOpt(Option(String)),
    aria_label: dataflow.NodeOpt(Option(String)),
    aria_level: dataflow.NodeOpt(Option(String)),
    aria_live: dataflow.NodeOpt(Option(AriaLive)),
    aria_modal: dataflow.NodeOpt(Option(Bool)),
    aria_multi_line: dataflow.NodeOpt(Option(Bool)),
    aria_multi_selectable: dataflow.NodeOpt(Option(Bool)),
    aria_orientation: dataflow.NodeOpt(Option(AriaOrientation)),
    aria_placeholder: dataflow.NodeOpt(Option(String)),
    aria_pos_in_set: dataflow.NodeOpt(Option(String)),
    aria_pressed: dataflow.NodeOpt(Option(AriaBoolMixedUndefined)),
    aria_read_only: dataflow.NodeOpt(Option(Bool)),
    aria_required: dataflow.NodeOpt(Option(Bool)),
    aria_role_description: dataflow.NodeOpt(Option(String)),
    aria_row_count: dataflow.NodeOpt(Option(String)),
    aria_row_index: dataflow.NodeOpt(Option(String)),
    aria_row_span: dataflow.NodeOpt(Option(String)),
    aria_selected: dataflow.NodeOpt(Option(AriaBoolUndefined)),
    aria_set_size: dataflow.NodeOpt(Option(String)),
    aria_sort: dataflow.NodeOpt(Option(AriaSort)),
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
    tab_index: dataflow.NodeOpt(Option(Int)),
    autofocus: dataflow.NodeOpt(Option(Bool)),
    form: dataflow.NodeOpt(Option(String)),
    form_action: dataflow.NodeOpt(Option(String)),
    form_enctype: dataflow.NodeOpt(Option(FormEnctype)),
    form_no_validate: dataflow.NodeOpt(Option(Bool)),
    form_target: dataflow.NodeOpt(Option(String)),
    name: dataflow.NodeOpt(Option(String)),
    value: dataflow.NodeOpt(Option(String)),
    disabled: dataflow.NodeOpt(Option(Bool)),
    form_method: dataflow.NodeOpt(Option(FormMethod)),
    style: dataflow.NodeOpt(styling.AtomOpt),
    events: markup_attributes.Events,
    on_add: Option(fragment.OnAdd),
    content: fragment.FragmentInput,
  )
}

pub type PlainButtonAttrs {
  PlainButtonAttrs(
    access_key: dataflow.NodeOpt(Option(String)),
    autocapitalize: dataflow.NodeOpt(Option(Autocapitalize)),
    content_editable: dataflow.NodeOpt(Option(ContentEditable)),
    dir: dataflow.NodeOpt(Option(HtmlDir)),
    draggable: dataflow.NodeOpt(Option(Bool)),
    enter_key_hint: dataflow.NodeOpt(Option(EnterKeyHint)),
    hidden: dataflow.NodeOpt(Option(HiddenValue)),
    inert: dataflow.NodeOpt(Option(Bool)),
    input_mode: dataflow.NodeOpt(Option(InputMode)),
    lang: dataflow.NodeOpt(Option(String)),
    spellcheck: dataflow.NodeOpt(Option(String)),
    translate: dataflow.NodeOpt(Option(Translate)),
    aria_atomic: dataflow.NodeOpt(Option(Bool)),
    aria_auto_complete: dataflow.NodeOpt(Option(AriaAutoComplete)),
    aria_busy: dataflow.NodeOpt(Option(Bool)),
    aria_checked: dataflow.NodeOpt(Option(AriaBoolMixedUndefined)),
    aria_col_count: dataflow.NodeOpt(Option(String)),
    aria_col_index: dataflow.NodeOpt(Option(String)),
    aria_col_span: dataflow.NodeOpt(Option(String)),
    aria_current: dataflow.NodeOpt(Option(AriaCurrent)),
    aria_description: dataflow.NodeOpt(Option(String)),
    aria_expanded: dataflow.NodeOpt(Option(AriaBoolUndefined)),
    aria_has_popup: dataflow.NodeOpt(Option(AriaHasPopup)),
    aria_hidden: dataflow.NodeOpt(Option(AriaBoolUndefined)),
    aria_invalid: dataflow.NodeOpt(Option(String)),
    aria_key_shortcuts: dataflow.NodeOpt(Option(String)),
    aria_label: dataflow.NodeOpt(Option(String)),
    aria_level: dataflow.NodeOpt(Option(String)),
    aria_live: dataflow.NodeOpt(Option(AriaLive)),
    aria_modal: dataflow.NodeOpt(Option(Bool)),
    aria_multi_line: dataflow.NodeOpt(Option(Bool)),
    aria_multi_selectable: dataflow.NodeOpt(Option(Bool)),
    aria_orientation: dataflow.NodeOpt(Option(AriaOrientation)),
    aria_placeholder: dataflow.NodeOpt(Option(String)),
    aria_pos_in_set: dataflow.NodeOpt(Option(String)),
    aria_pressed: dataflow.NodeOpt(Option(AriaBoolMixedUndefined)),
    aria_read_only: dataflow.NodeOpt(Option(Bool)),
    aria_required: dataflow.NodeOpt(Option(Bool)),
    aria_role_description: dataflow.NodeOpt(Option(String)),
    aria_row_count: dataflow.NodeOpt(Option(String)),
    aria_row_index: dataflow.NodeOpt(Option(String)),
    aria_row_span: dataflow.NodeOpt(Option(String)),
    aria_selected: dataflow.NodeOpt(Option(AriaBoolUndefined)),
    aria_set_size: dataflow.NodeOpt(Option(String)),
    aria_sort: dataflow.NodeOpt(Option(AriaSort)),
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
    tab_index: dataflow.NodeOpt(Option(Int)),
    autofocus: dataflow.NodeOpt(Option(Bool)),
    disabled: dataflow.NodeOpt(Option(Bool)),
    style: dataflow.NodeOpt(styling.AtomOpt),
    events: markup_attributes.Events,
    on_add: Option(fragment.OnAdd),
    content: fragment.FragmentInput,
  )
}

pub type ResetButtonAttrs {
  ResetButtonAttrs(
    access_key: dataflow.NodeOpt(Option(String)),
    autocapitalize: dataflow.NodeOpt(Option(Autocapitalize)),
    content_editable: dataflow.NodeOpt(Option(ContentEditable)),
    dir: dataflow.NodeOpt(Option(HtmlDir)),
    draggable: dataflow.NodeOpt(Option(Bool)),
    enter_key_hint: dataflow.NodeOpt(Option(EnterKeyHint)),
    hidden: dataflow.NodeOpt(Option(HiddenValue)),
    inert: dataflow.NodeOpt(Option(Bool)),
    input_mode: dataflow.NodeOpt(Option(InputMode)),
    lang: dataflow.NodeOpt(Option(String)),
    spellcheck: dataflow.NodeOpt(Option(String)),
    translate: dataflow.NodeOpt(Option(Translate)),
    aria_atomic: dataflow.NodeOpt(Option(Bool)),
    aria_auto_complete: dataflow.NodeOpt(Option(AriaAutoComplete)),
    aria_busy: dataflow.NodeOpt(Option(Bool)),
    aria_checked: dataflow.NodeOpt(Option(AriaBoolMixedUndefined)),
    aria_col_count: dataflow.NodeOpt(Option(String)),
    aria_col_index: dataflow.NodeOpt(Option(String)),
    aria_col_span: dataflow.NodeOpt(Option(String)),
    aria_current: dataflow.NodeOpt(Option(AriaCurrent)),
    aria_description: dataflow.NodeOpt(Option(String)),
    aria_expanded: dataflow.NodeOpt(Option(AriaBoolUndefined)),
    aria_has_popup: dataflow.NodeOpt(Option(AriaHasPopup)),
    aria_hidden: dataflow.NodeOpt(Option(AriaBoolUndefined)),
    aria_invalid: dataflow.NodeOpt(Option(String)),
    aria_key_shortcuts: dataflow.NodeOpt(Option(String)),
    aria_label: dataflow.NodeOpt(Option(String)),
    aria_level: dataflow.NodeOpt(Option(String)),
    aria_live: dataflow.NodeOpt(Option(AriaLive)),
    aria_modal: dataflow.NodeOpt(Option(Bool)),
    aria_multi_line: dataflow.NodeOpt(Option(Bool)),
    aria_multi_selectable: dataflow.NodeOpt(Option(Bool)),
    aria_orientation: dataflow.NodeOpt(Option(AriaOrientation)),
    aria_placeholder: dataflow.NodeOpt(Option(String)),
    aria_pos_in_set: dataflow.NodeOpt(Option(String)),
    aria_pressed: dataflow.NodeOpt(Option(AriaBoolMixedUndefined)),
    aria_read_only: dataflow.NodeOpt(Option(Bool)),
    aria_required: dataflow.NodeOpt(Option(Bool)),
    aria_role_description: dataflow.NodeOpt(Option(String)),
    aria_row_count: dataflow.NodeOpt(Option(String)),
    aria_row_index: dataflow.NodeOpt(Option(String)),
    aria_row_span: dataflow.NodeOpt(Option(String)),
    aria_selected: dataflow.NodeOpt(Option(AriaBoolUndefined)),
    aria_set_size: dataflow.NodeOpt(Option(String)),
    aria_sort: dataflow.NodeOpt(Option(AriaSort)),
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
    tab_index: dataflow.NodeOpt(Option(Int)),
    autofocus: dataflow.NodeOpt(Option(Bool)),
    form: dataflow.NodeOpt(Option(String)),
    disabled: dataflow.NodeOpt(Option(Bool)),
    style: dataflow.NodeOpt(styling.AtomOpt),
    events: markup_attributes.Events,
    on_add: Option(fragment.OnAdd),
    content: fragment.FragmentInput,
  )
}

pub fn submit_button_attrs() -> SubmitButtonAttrs {
  SubmitButtonAttrs(
    access_key: dataflow.literal(None),
    autocapitalize: dataflow.literal(None),
    content_editable: dataflow.literal(None),
    dir: dataflow.literal(None),
    draggable: dataflow.literal(None),
    enter_key_hint: dataflow.literal(None),
    hidden: dataflow.literal(None),
    inert: dataflow.literal(None),
    input_mode: dataflow.literal(None),
    lang: dataflow.literal(None),
    spellcheck: dataflow.literal(None),
    translate: dataflow.literal(None),
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
    title: dataflow.literal(None),
    nonce: dataflow.literal(None),
    role: dataflow.literal(None),
    slot: dataflow.literal(None),
    tab_index: dataflow.literal(None),
    autofocus: dataflow.literal(None),
    form: dataflow.literal(None),
    form_action: dataflow.literal(None),
    form_enctype: dataflow.literal(None),
    form_no_validate: dataflow.literal(None),
    form_target: dataflow.literal(None),
    name: dataflow.literal(None),
    value: dataflow.literal(None),
    disabled: dataflow.literal(None),
    form_method: dataflow.literal(None),
    style: dataflow.literal(styling.empty()),
    content: fragment.EmptyInput,
    events: markup_attributes.events(),
    on_add: None,
  )
}

pub fn plain_button_attrs() -> PlainButtonAttrs {
  PlainButtonAttrs(
    access_key: dataflow.literal(None),
    autocapitalize: dataflow.literal(None),
    content_editable: dataflow.literal(None),
    dir: dataflow.literal(None),
    draggable: dataflow.literal(None),
    enter_key_hint: dataflow.literal(None),
    hidden: dataflow.literal(None),
    inert: dataflow.literal(None),
    input_mode: dataflow.literal(None),
    lang: dataflow.literal(None),
    spellcheck: dataflow.literal(None),
    translate: dataflow.literal(None),
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
    title: dataflow.literal(None),
    nonce: dataflow.literal(None),
    role: dataflow.literal(None),
    slot: dataflow.literal(None),
    tab_index: dataflow.literal(None),
    autofocus: dataflow.literal(None),
    disabled: dataflow.literal(None),
    style: dataflow.literal(styling.empty()),
    content: fragment.EmptyInput,
    events: markup_attributes.events(),
    on_add: None,
  )
}

pub fn reset_button_attrs() -> ResetButtonAttrs {
  ResetButtonAttrs(
    access_key: dataflow.literal(None),
    autocapitalize: dataflow.literal(None),
    content_editable: dataflow.literal(None),
    dir: dataflow.literal(None),
    draggable: dataflow.literal(None),
    enter_key_hint: dataflow.literal(None),
    hidden: dataflow.literal(None),
    inert: dataflow.literal(None),
    input_mode: dataflow.literal(None),
    lang: dataflow.literal(None),
    spellcheck: dataflow.literal(None),
    translate: dataflow.literal(None),
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
    title: dataflow.literal(None),
    nonce: dataflow.literal(None),
    role: dataflow.literal(None),
    slot: dataflow.literal(None),
    tab_index: dataflow.literal(None),
    autofocus: dataflow.literal(None),
    form: dataflow.literal(None),
    disabled: dataflow.literal(None),
    style: dataflow.literal(styling.empty()),
    content: fragment.EmptyInput,
    events: markup_attributes.events(),
    on_add: None,
  )
}

pub fn submit_button(attrs: SubmitButtonAttrs) -> ButtonAttrs {
  SubmitButton(attrs)
}

pub fn plain_button(attrs: PlainButtonAttrs) -> ButtonAttrs {
  PlainButton(attrs)
}

pub fn reset_button(attrs: ResetButtonAttrs) -> ButtonAttrs {
  ResetButton(attrs)
}

pub fn button(attrs: ButtonAttrs) -> fragment.Fragment {
  case attrs {
    SubmitButton(attrs) -> {
      let SubmitButtonAttrs(
        access_key,
        autocapitalize,
        content_editable,
        dir,
        draggable,
        enter_key_hint,
        hidden,
        inert,
        input_mode,
        lang,
        spellcheck,
        translate,
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
        title,
        nonce,
        role,
        slot,
        tab_index,
        autofocus,
        form,
        form_action,
        form_enctype,
        form_no_validate,
        form_target,
        name,
        value,
        disabled,
        form_method,
        style,
        events,
        on_add,
        content,
      ) = attrs
      let common =
        common_html(
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
          access_key,
          autocapitalize,
          content_editable,
          dir,
          draggable,
          enter_key_hint,
          hidden,
          inert,
          input_mode,
          lang,
          spellcheck,
          translate,
          id,
          aria_disabled,
          class_names,
          title,
          nonce,
          role,
          slot,
          tab_index,
          autofocus,
          events,
        )
      render_button(
        common,
        form,
        form_action,
        form_enctype,
        form_no_validate,
        form_target,
        name,
        value,
        disabled,
        form_method,
        "submit",
        style,
        events,
        on_add,
        content,
      )
    }
    PlainButton(attrs) -> {
      let PlainButtonAttrs(
        access_key,
        autocapitalize,
        content_editable,
        dir,
        draggable,
        enter_key_hint,
        hidden,
        inert,
        input_mode,
        lang,
        spellcheck,
        translate,
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
        title,
        nonce,
        role,
        slot,
        tab_index,
        autofocus,
        disabled,
        style,
        events,
        on_add,
        content,
      ) = attrs
      let common =
        common_html(
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
          access_key,
          autocapitalize,
          content_editable,
          dir,
          draggable,
          enter_key_hint,
          hidden,
          inert,
          input_mode,
          lang,
          spellcheck,
          translate,
          id,
          aria_disabled,
          class_names,
          title,
          nonce,
          role,
          slot,
          tab_index,
          autofocus,
          events,
        )
      render_button(
        common,
        dataflow.literal(None),
        dataflow.literal(None),
        dataflow.literal(None),
        dataflow.literal(None),
        dataflow.literal(None),
        dataflow.literal(None),
        dataflow.literal(None),
        disabled,
        dataflow.literal(None),
        "button",
        style,
        events,
        on_add,
        content,
      )
    }
    ResetButton(attrs) -> {
      let ResetButtonAttrs(
        access_key,
        autocapitalize,
        content_editable,
        dir,
        draggable,
        enter_key_hint,
        hidden,
        inert,
        input_mode,
        lang,
        spellcheck,
        translate,
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
        title,
        nonce,
        role,
        slot,
        tab_index,
        autofocus,
        form,
        disabled,
        style,
        events,
        on_add,
        content,
      ) = attrs
      let common =
        common_html(
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
          access_key,
          autocapitalize,
          content_editable,
          dir,
          draggable,
          enter_key_hint,
          hidden,
          inert,
          input_mode,
          lang,
          spellcheck,
          translate,
          id,
          aria_disabled,
          class_names,
          title,
          nonce,
          role,
          slot,
          tab_index,
          autofocus,
          events,
        )
      render_button(
        common,
        form,
        dataflow.literal(None),
        dataflow.literal(None),
        dataflow.literal(None),
        dataflow.literal(None),
        dataflow.literal(None),
        dataflow.literal(None),
        disabled,
        dataflow.literal(None),
        "reset",
        style,
        events,
        on_add,
        content,
      )
    }
  }
}

pub fn portal_button(
  node: fragment.Node,
  attrs: ButtonAttrs,
) -> fragment.Fragment {
  to_portal(node, button(attrs))
}

fn render_button(
  common: List(fragment.Attribute),
  form: dataflow.NodeOpt(Option(String)),
  form_action: dataflow.NodeOpt(Option(String)),
  form_enctype: dataflow.NodeOpt(Option(FormEnctype)),
  form_no_validate: dataflow.NodeOpt(Option(Bool)),
  form_target: dataflow.NodeOpt(Option(String)),
  name: dataflow.NodeOpt(Option(String)),
  value: dataflow.NodeOpt(Option(String)),
  disabled: dataflow.NodeOpt(Option(Bool)),
  form_method: dataflow.NodeOpt(Option(FormMethod)),
  type_: String,
  style: dataflow.NodeOpt(styling.AtomOpt),
  events: markup_attributes.Events,
  on_add: Option(fragment.OnAdd),
  content: fragment.FragmentInput,
) -> fragment.Fragment {
  let attributes = common
  let attributes =
    list.append(attributes, [
      fragment.ReactiveOptionalStringAttribute("form", form),
      fragment.ReactiveOptionalStringAttribute("formaction", form_action),
      fragment.ReactiveOptionalStringAttribute(
        "formenctype",
        markup_attributes.map_optional(form_enctype, form_enctype_text),
      ),
      fragment.ReactiveOptionalBooleanAttribute(
        "formnovalidate",
        form_no_validate,
      ),
      fragment.ReactiveOptionalStringAttribute("formtarget", form_target),
      fragment.ReactiveOptionalStringAttribute("name", name),
      fragment.ReactiveOptionalStringAttribute("value", value),
    ])
  let attributes =
    list.append(attributes, [
      fragment.ReactiveOptionalBooleanAttribute("disabled", disabled),
    ])
  let attributes =
    list.append(attributes, [
      fragment.ReactiveOptionalStringAttribute(
        "formmethod",
        markup_attributes.map_optional(form_method, form_method_text),
      ),
    ])
  let attributes = markup_attributes.add_string(attributes, "type", Some(type_))
  let attributes = markup_attributes.add_style(attributes, style)
  let attributes = markup_attributes.add_events(attributes, events)
  let attributes = markup_attributes.add_on_add(attributes, on_add)
  fragment.element("button", attributes, content)
}

pub type InputAttrs {
  Checkbox(CheckboxAttrs)
  TextInput(TextInputAttrs)
}

pub type Autocomplete {
  AutocompleteOff
  AutocompleteOn
  AutocompleteTokens(List(AutocompleteToken))
}

pub type AutocompleteToken {
  AutocompleteEmail
  AutocompleteName
  AutocompleteSection(String)
}

pub type CheckboxAttrs {
  CheckboxAttrs(
    access_key: dataflow.NodeOpt(Option(String)),
    autocapitalize: dataflow.NodeOpt(Option(Autocapitalize)),
    content_editable: dataflow.NodeOpt(Option(ContentEditable)),
    dir: dataflow.NodeOpt(Option(HtmlDir)),
    draggable: dataflow.NodeOpt(Option(Bool)),
    enter_key_hint: dataflow.NodeOpt(Option(EnterKeyHint)),
    hidden: dataflow.NodeOpt(Option(HiddenValue)),
    inert: dataflow.NodeOpt(Option(Bool)),
    input_mode: dataflow.NodeOpt(Option(InputMode)),
    lang: dataflow.NodeOpt(Option(String)),
    spellcheck: dataflow.NodeOpt(Option(String)),
    translate: dataflow.NodeOpt(Option(Translate)),
    aria_atomic: dataflow.NodeOpt(Option(Bool)),
    aria_auto_complete: dataflow.NodeOpt(Option(AriaAutoComplete)),
    aria_busy: dataflow.NodeOpt(Option(Bool)),
    aria_checked: dataflow.NodeOpt(Option(AriaBoolMixedUndefined)),
    aria_col_count: dataflow.NodeOpt(Option(String)),
    aria_col_index: dataflow.NodeOpt(Option(String)),
    aria_col_span: dataflow.NodeOpt(Option(String)),
    aria_current: dataflow.NodeOpt(Option(AriaCurrent)),
    aria_description: dataflow.NodeOpt(Option(String)),
    aria_expanded: dataflow.NodeOpt(Option(AriaBoolUndefined)),
    aria_has_popup: dataflow.NodeOpt(Option(AriaHasPopup)),
    aria_hidden: dataflow.NodeOpt(Option(AriaBoolUndefined)),
    aria_invalid: dataflow.NodeOpt(Option(String)),
    aria_key_shortcuts: dataflow.NodeOpt(Option(String)),
    aria_label: dataflow.NodeOpt(Option(String)),
    aria_level: dataflow.NodeOpt(Option(String)),
    aria_live: dataflow.NodeOpt(Option(AriaLive)),
    aria_modal: dataflow.NodeOpt(Option(Bool)),
    aria_multi_line: dataflow.NodeOpt(Option(Bool)),
    aria_multi_selectable: dataflow.NodeOpt(Option(Bool)),
    aria_orientation: dataflow.NodeOpt(Option(AriaOrientation)),
    aria_placeholder: dataflow.NodeOpt(Option(String)),
    aria_pos_in_set: dataflow.NodeOpt(Option(String)),
    aria_pressed: dataflow.NodeOpt(Option(AriaBoolMixedUndefined)),
    aria_read_only: dataflow.NodeOpt(Option(Bool)),
    aria_required: dataflow.NodeOpt(Option(Bool)),
    aria_role_description: dataflow.NodeOpt(Option(String)),
    aria_row_count: dataflow.NodeOpt(Option(String)),
    aria_row_index: dataflow.NodeOpt(Option(String)),
    aria_row_span: dataflow.NodeOpt(Option(String)),
    aria_selected: dataflow.NodeOpt(Option(AriaBoolUndefined)),
    aria_set_size: dataflow.NodeOpt(Option(String)),
    aria_sort: dataflow.NodeOpt(Option(AriaSort)),
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
    tab_index: dataflow.NodeOpt(Option(Int)),
    autofocus: dataflow.NodeOpt(Option(Bool)),
    form: dataflow.NodeOpt(Option(String)),
    required: dataflow.NodeOpt(Option(Bool)),
    checked: dataflow.NodeOpt(Option(Bool)),
    disabled: dataflow.NodeOpt(Option(Bool)),
    name: dataflow.NodeOpt(Option(String)),
    value: dataflow.NodeOpt(Option(String)),
    style: dataflow.NodeOpt(styling.AtomOpt),
    events: markup_attributes.Events,
    on_add: Option(fragment.OnAdd),
    content: fragment.FragmentInput,
  )
}

pub fn checkbox_attrs() -> CheckboxAttrs {
  CheckboxAttrs(
    access_key: dataflow.literal(None),
    autocapitalize: dataflow.literal(None),
    content_editable: dataflow.literal(None),
    dir: dataflow.literal(None),
    draggable: dataflow.literal(None),
    enter_key_hint: dataflow.literal(None),
    hidden: dataflow.literal(None),
    inert: dataflow.literal(None),
    input_mode: dataflow.literal(None),
    lang: dataflow.literal(None),
    spellcheck: dataflow.literal(None),
    translate: dataflow.literal(None),
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
    title: dataflow.literal(None),
    nonce: dataflow.literal(None),
    role: dataflow.literal(None),
    slot: dataflow.literal(None),
    tab_index: dataflow.literal(None),
    autofocus: dataflow.literal(None),
    form: dataflow.literal(None),
    required: dataflow.literal(None),
    checked: dataflow.literal(None),
    disabled: dataflow.literal(None),
    name: dataflow.literal(None),
    value: dataflow.literal(None),
    style: dataflow.literal(styling.empty()),
    events: markup_attributes.events(),
    on_add: None,
    content: fragment.EmptyInput,
  )
}

pub fn checkbox(attrs: CheckboxAttrs) -> InputAttrs {
  Checkbox(attrs)
}

pub type TextInputAttrs {
  TextInputAttrs(
    access_key: dataflow.NodeOpt(Option(String)),
    autocapitalize: dataflow.NodeOpt(Option(Autocapitalize)),
    content_editable: dataflow.NodeOpt(Option(ContentEditable)),
    dir: dataflow.NodeOpt(Option(HtmlDir)),
    draggable: dataflow.NodeOpt(Option(Bool)),
    enter_key_hint: dataflow.NodeOpt(Option(EnterKeyHint)),
    hidden: dataflow.NodeOpt(Option(HiddenValue)),
    inert: dataflow.NodeOpt(Option(Bool)),
    input_mode: dataflow.NodeOpt(Option(InputMode)),
    lang: dataflow.NodeOpt(Option(String)),
    spellcheck: dataflow.NodeOpt(Option(String)),
    translate: dataflow.NodeOpt(Option(Translate)),
    aria_atomic: dataflow.NodeOpt(Option(Bool)),
    aria_auto_complete: dataflow.NodeOpt(Option(AriaAutoComplete)),
    aria_busy: dataflow.NodeOpt(Option(Bool)),
    aria_checked: dataflow.NodeOpt(Option(AriaBoolMixedUndefined)),
    aria_col_count: dataflow.NodeOpt(Option(String)),
    aria_col_index: dataflow.NodeOpt(Option(String)),
    aria_col_span: dataflow.NodeOpt(Option(String)),
    aria_current: dataflow.NodeOpt(Option(AriaCurrent)),
    aria_description: dataflow.NodeOpt(Option(String)),
    aria_expanded: dataflow.NodeOpt(Option(AriaBoolUndefined)),
    aria_has_popup: dataflow.NodeOpt(Option(AriaHasPopup)),
    aria_hidden: dataflow.NodeOpt(Option(AriaBoolUndefined)),
    aria_invalid: dataflow.NodeOpt(Option(String)),
    aria_key_shortcuts: dataflow.NodeOpt(Option(String)),
    aria_label: dataflow.NodeOpt(Option(String)),
    aria_level: dataflow.NodeOpt(Option(String)),
    aria_live: dataflow.NodeOpt(Option(AriaLive)),
    aria_modal: dataflow.NodeOpt(Option(Bool)),
    aria_multi_line: dataflow.NodeOpt(Option(Bool)),
    aria_multi_selectable: dataflow.NodeOpt(Option(Bool)),
    aria_orientation: dataflow.NodeOpt(Option(AriaOrientation)),
    aria_placeholder: dataflow.NodeOpt(Option(String)),
    aria_pos_in_set: dataflow.NodeOpt(Option(String)),
    aria_pressed: dataflow.NodeOpt(Option(AriaBoolMixedUndefined)),
    aria_read_only: dataflow.NodeOpt(Option(Bool)),
    aria_required: dataflow.NodeOpt(Option(Bool)),
    aria_role_description: dataflow.NodeOpt(Option(String)),
    aria_row_count: dataflow.NodeOpt(Option(String)),
    aria_row_index: dataflow.NodeOpt(Option(String)),
    aria_row_span: dataflow.NodeOpt(Option(String)),
    aria_selected: dataflow.NodeOpt(Option(AriaBoolUndefined)),
    aria_set_size: dataflow.NodeOpt(Option(String)),
    aria_sort: dataflow.NodeOpt(Option(AriaSort)),
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
    tab_index: dataflow.NodeOpt(Option(Int)),
    autofocus: dataflow.NodeOpt(Option(Bool)),
    form: dataflow.NodeOpt(Option(String)),
    list: dataflow.NodeOpt(Option(String)),
    min_length: dataflow.NodeOpt(Option(Int)),
    pattern: dataflow.NodeOpt(Option(String)),
    autocomplete: dataflow.NodeOpt(Option(Autocomplete)),
    max_length: dataflow.NodeOpt(Option(Int)),
    name: dataflow.NodeOpt(Option(String)),
    placeholder: dataflow.NodeOpt(Option(String)),
    read_only: dataflow.NodeOpt(Option(Bool)),
    required: dataflow.NodeOpt(Option(Bool)),
    size: dataflow.NodeOpt(Option(Int)),
    value: dataflow.NodeOpt(Option(String)),
    disabled: dataflow.NodeOpt(Option(Bool)),
    style: dataflow.NodeOpt(styling.AtomOpt),
    events: markup_attributes.Events,
    on_add: Option(fragment.OnAdd),
    content: fragment.FragmentInput,
  )
}

pub fn text_input_attrs() -> TextInputAttrs {
  TextInputAttrs(
    access_key: dataflow.literal(None),
    autocapitalize: dataflow.literal(None),
    content_editable: dataflow.literal(None),
    dir: dataflow.literal(None),
    draggable: dataflow.literal(None),
    enter_key_hint: dataflow.literal(None),
    hidden: dataflow.literal(None),
    inert: dataflow.literal(None),
    input_mode: dataflow.literal(None),
    lang: dataflow.literal(None),
    spellcheck: dataflow.literal(None),
    translate: dataflow.literal(None),
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
    title: dataflow.literal(None),
    nonce: dataflow.literal(None),
    role: dataflow.literal(None),
    slot: dataflow.literal(None),
    tab_index: dataflow.literal(None),
    autofocus: dataflow.literal(None),
    form: dataflow.literal(None),
    list: dataflow.literal(None),
    min_length: dataflow.literal(None),
    pattern: dataflow.literal(None),
    autocomplete: dataflow.literal(None),
    max_length: dataflow.literal(None),
    name: dataflow.literal(None),
    placeholder: dataflow.literal(None),
    read_only: dataflow.literal(None),
    required: dataflow.literal(None),
    size: dataflow.literal(None),
    value: dataflow.literal(None),
    disabled: dataflow.literal(None),
    style: dataflow.literal(styling.empty()),
    events: markup_attributes.events(),
    on_add: None,
    content: fragment.EmptyInput,
  )
}

pub fn text_input(attrs: TextInputAttrs) -> InputAttrs {
  TextInput(attrs)
}

pub fn input(attrs: InputAttrs) -> fragment.Fragment {
  case attrs {
    Checkbox(attrs) -> {
      let CheckboxAttrs(
        access_key,
        autocapitalize,
        content_editable,
        dir,
        draggable,
        enter_key_hint,
        hidden,
        inert,
        input_mode,
        lang,
        spellcheck,
        translate,
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
        title,
        nonce,
        role,
        slot,
        tab_index,
        autofocus,
        form,
        required,
        checked,
        disabled,
        name,
        value,
        style,
        events,
        on_add,
        content,
      ) = attrs
      let attributes =
        common_html(
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
          access_key,
          autocapitalize,
          content_editable,
          dir,
          draggable,
          enter_key_hint,
          hidden,
          inert,
          input_mode,
          lang,
          spellcheck,
          translate,
          id,
          aria_disabled,
          class_names,
          title,
          nonce,
          role,
          slot,
          tab_index,
          autofocus,
          events,
        )
      let attributes =
        list.append(attributes, [
          fragment.ReactiveOptionalStringAttribute("form", form),
        ])
      let attributes =
        list.append(attributes, [
          fragment.ReactiveOptionalBooleanAttribute("required", required),
          fragment.ReactiveOptionalBooleanAttribute("checked", checked),
          fragment.ReactiveOptionalBooleanAttribute("disabled", disabled),
        ])
      let attributes =
        list.append(attributes, [
          fragment.ReactiveOptionalStringAttribute("name", name),
          fragment.ReactiveOptionalStringAttribute("value", value),
        ])
      let attributes =
        markup_attributes.add_string(attributes, "type", Some("checkbox"))
      let attributes = markup_attributes.add_style(attributes, style)
      let attributes = markup_attributes.add_events(attributes, events)
      let attributes = markup_attributes.add_on_add(attributes, on_add)
      fragment.element("input", attributes, content)
    }
    TextInput(attrs) -> {
      let TextInputAttrs(
        access_key,
        autocapitalize,
        content_editable,
        dir,
        draggable,
        enter_key_hint,
        hidden,
        inert,
        input_mode,
        lang,
        spellcheck,
        translate,
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
        title,
        nonce,
        role,
        slot,
        tab_index,
        autofocus,
        form,
        list,
        min_length,
        pattern,
        autocomplete,
        max_length,
        name,
        placeholder,
        read_only,
        required,
        size,
        value,
        disabled,
        style,
        events,
        on_add,
        content,
      ) = attrs
      let attributes =
        common_html(
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
          access_key,
          autocapitalize,
          content_editable,
          dir,
          draggable,
          enter_key_hint,
          hidden,
          inert,
          input_mode,
          lang,
          spellcheck,
          translate,
          id,
          aria_disabled,
          class_names,
          title,
          nonce,
          role,
          slot,
          tab_index,
          autofocus,
          events,
        )
      let attributes =
        list.append(attributes, [
          fragment.ReactiveOptionalStringAttribute("form", form),
          fragment.ReactiveOptionalStringAttribute("list", list),
          fragment.ReactiveOptionalStringAttribute(
            "minlength",
            markup_attributes.map_optional(min_length, int.to_string),
          ),
          fragment.ReactiveOptionalStringAttribute("pattern", pattern),
          fragment.ReactiveOptionalStringAttribute(
            "autocomplete",
            markup_attributes.map_optional(autocomplete, autocomplete_text),
          ),
          fragment.ReactiveOptionalStringAttribute(
            "maxlength",
            markup_attributes.map_optional(max_length, int.to_string),
          ),
          fragment.ReactiveOptionalStringAttribute("name", name),
          fragment.ReactiveOptionalStringAttribute("placeholder", placeholder),
          fragment.ReactiveOptionalBooleanAttribute("readonly", read_only),
          fragment.ReactiveOptionalBooleanAttribute("required", required),
          fragment.ReactiveOptionalStringAttribute(
            "size",
            markup_attributes.map_optional(size, int.to_string),
          ),
          fragment.ReactiveOptionalStringAttribute("value", value),
          fragment.ReactiveOptionalBooleanAttribute("disabled", disabled),
        ])
      let attributes =
        markup_attributes.add_string(attributes, "type", Some("text"))
      let attributes = markup_attributes.add_style(attributes, style)
      let attributes = markup_attributes.add_events(attributes, events)
      let attributes = markup_attributes.add_on_add(attributes, on_add)
      fragment.element("input", attributes, content)
    }
  }
}

pub fn portal_input(
  node: fragment.Node,
  attrs: InputAttrs,
) -> fragment.Fragment {
  to_portal(node, input(attrs))
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

fn autocomplete_text(value: Autocomplete) -> String {
  case value {
    AutocompleteOff -> "off"
    AutocompleteOn -> "on"
    AutocompleteTokens(values) -> autocomplete_tokens_text(values)
  }
}

fn common_html(
  aria_atomic: dataflow.NodeOpt(Option(Bool)),
  aria_auto_complete: dataflow.NodeOpt(Option(AriaAutoComplete)),
  aria_busy: dataflow.NodeOpt(Option(Bool)),
  aria_checked: dataflow.NodeOpt(Option(AriaBoolMixedUndefined)),
  aria_col_count: dataflow.NodeOpt(Option(String)),
  aria_col_index: dataflow.NodeOpt(Option(String)),
  aria_col_span: dataflow.NodeOpt(Option(String)),
  aria_current: dataflow.NodeOpt(Option(AriaCurrent)),
  aria_description: dataflow.NodeOpt(Option(String)),
  aria_expanded: dataflow.NodeOpt(Option(AriaBoolUndefined)),
  aria_has_popup: dataflow.NodeOpt(Option(AriaHasPopup)),
  aria_hidden: dataflow.NodeOpt(Option(AriaBoolUndefined)),
  aria_invalid: dataflow.NodeOpt(Option(String)),
  aria_key_shortcuts: dataflow.NodeOpt(Option(String)),
  aria_label: dataflow.NodeOpt(Option(String)),
  aria_level: dataflow.NodeOpt(Option(String)),
  aria_live: dataflow.NodeOpt(Option(AriaLive)),
  aria_modal: dataflow.NodeOpt(Option(Bool)),
  aria_multi_line: dataflow.NodeOpt(Option(Bool)),
  aria_multi_selectable: dataflow.NodeOpt(Option(Bool)),
  aria_orientation: dataflow.NodeOpt(Option(AriaOrientation)),
  aria_placeholder: dataflow.NodeOpt(Option(String)),
  aria_pos_in_set: dataflow.NodeOpt(Option(String)),
  aria_pressed: dataflow.NodeOpt(Option(AriaBoolMixedUndefined)),
  aria_read_only: dataflow.NodeOpt(Option(Bool)),
  aria_required: dataflow.NodeOpt(Option(Bool)),
  aria_role_description: dataflow.NodeOpt(Option(String)),
  aria_row_count: dataflow.NodeOpt(Option(String)),
  aria_row_index: dataflow.NodeOpt(Option(String)),
  aria_row_span: dataflow.NodeOpt(Option(String)),
  aria_selected: dataflow.NodeOpt(Option(AriaBoolUndefined)),
  aria_set_size: dataflow.NodeOpt(Option(String)),
  aria_sort: dataflow.NodeOpt(Option(AriaSort)),
  aria_value_max: dataflow.NodeOpt(Option(String)),
  aria_value_min: dataflow.NodeOpt(Option(String)),
  aria_value_now: dataflow.NodeOpt(Option(String)),
  aria_value_text: dataflow.NodeOpt(Option(String)),
  access_key: dataflow.NodeOpt(Option(String)),
  autocapitalize: dataflow.NodeOpt(Option(Autocapitalize)),
  content_editable: dataflow.NodeOpt(Option(ContentEditable)),
  dir: dataflow.NodeOpt(Option(HtmlDir)),
  draggable: dataflow.NodeOpt(Option(Bool)),
  enter_key_hint: dataflow.NodeOpt(Option(EnterKeyHint)),
  hidden: dataflow.NodeOpt(Option(HiddenValue)),
  inert: dataflow.NodeOpt(Option(Bool)),
  input_mode: dataflow.NodeOpt(Option(InputMode)),
  lang: dataflow.NodeOpt(Option(String)),
  spellcheck: dataflow.NodeOpt(Option(String)),
  translate: dataflow.NodeOpt(Option(Translate)),
  id: dataflow.NodeOpt(Option(String)),
  aria_disabled: dataflow.NodeOpt(Option(Bool)),
  class_names: dataflow.NodeOpt(Option(List(String))),
  title: dataflow.NodeOpt(Option(String)),
  nonce: dataflow.NodeOpt(Option(String)),
  role: dataflow.NodeOpt(Option(String)),
  slot: dataflow.NodeOpt(Option(String)),
  tab_index: dataflow.NodeOpt(Option(Int)),
  autofocus: dataflow.NodeOpt(Option(Bool)),
  events: markup_attributes.Events,
) -> List(fragment.Attribute) {
  markup_attributes.common_reactive(
    access_key,
    markup_attributes.map_optional(autocapitalize, autocapitalize_text),
    markup_attributes.map_optional(content_editable, content_editable_text),
    markup_attributes.map_optional(dir, dir_text),
    markup_attributes.map_optional(draggable, markup_attributes.bool_text),
    markup_attributes.map_optional(enter_key_hint, enter_key_hint_text),
    markup_attributes.map_optional(hidden, hidden_text),
    inert,
    markup_attributes.map_optional(input_mode, input_mode_text),
    lang,
    markup_attributes.map_optional(aria_atomic, markup_attributes.bool_text),
    markup_attributes.map_optional(aria_auto_complete, aria_auto_complete_text),
    markup_attributes.map_optional(aria_busy, markup_attributes.bool_text),
    markup_attributes.map_optional(aria_checked, aria_bool_mixed_undefined_text),
    aria_col_count,
    aria_col_index,
    aria_col_span,
    markup_attributes.map_optional(aria_current, aria_current_text),
    aria_description,
    markup_attributes.map_optional(aria_expanded, aria_bool_undefined_text),
    markup_attributes.map_optional(aria_has_popup, aria_has_popup_text),
    markup_attributes.map_optional(aria_hidden, aria_bool_undefined_text),
    aria_invalid,
    aria_key_shortcuts,
    aria_label,
    aria_level,
    markup_attributes.map_optional(aria_live, aria_live_text),
    markup_attributes.map_optional(aria_modal, markup_attributes.bool_text),
    markup_attributes.map_optional(aria_multi_line, markup_attributes.bool_text),
    markup_attributes.map_optional(
      aria_multi_selectable,
      markup_attributes.bool_text,
    ),
    markup_attributes.map_optional(aria_orientation, aria_orientation_text),
    aria_placeholder,
    aria_pos_in_set,
    markup_attributes.map_optional(aria_pressed, aria_bool_mixed_undefined_text),
    markup_attributes.map_optional(aria_read_only, markup_attributes.bool_text),
    markup_attributes.map_optional(aria_required, markup_attributes.bool_text),
    aria_role_description,
    aria_row_count,
    aria_row_index,
    aria_row_span,
    markup_attributes.map_optional(aria_selected, aria_bool_undefined_text),
    aria_set_size,
    markup_attributes.map_optional(aria_sort, aria_sort_text),
    aria_value_max,
    aria_value_min,
    aria_value_now,
    aria_value_text,
    id,
    aria_disabled,
    class_names,
    title,
    nonce,
    role,
    slot,
    spellcheck,
    tab_index,
    autofocus,
    markup_attributes.map_optional(translate, translate_text),
    events,
  )
}

fn form_enctype_text(value: FormEnctype) -> String {
  case value {
    FormUrlEncoded -> "application/x-www-form-urlencoded"
    FormMultipart -> "multipart/form-data"
    FormTextPlain -> "text/plain"
  }
}

fn form_method_text(value: FormMethod) -> String {
  case value {
    FormDialog -> "dialog"
    FormGet -> "get"
    FormPost -> "post"
  }
}

fn autocomplete_tokens_text(values: List(AutocompleteToken)) -> String {
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

fn autocomplete_token_text(value: AutocompleteToken) -> String {
  case value {
    AutocompleteEmail -> "email"
    AutocompleteName -> "name"
    AutocompleteSection(value) -> "section-" <> value
  }
}
