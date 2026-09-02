import lib_markup_attributes as attr
import lib_markup_html as html

pub fn auto_complete(value: html.AriaAutoComplete) -> String {
  case value {
    html.AriaAutoCompleteBoth -> "both"
    html.AriaAutoCompleteInline -> "inline"
    html.AriaAutoCompleteList -> "list"
    html.AriaAutoCompleteNone -> "none"
  }
}

pub fn bool_undefined(value: html.AriaBoolUndefined) -> String {
  case value {
    html.AriaBool(value) -> attr.bool_text(value)
    html.AriaUndefined -> "undefined"
  }
}

pub fn bool_mixed_undefined(value: html.AriaBoolMixedUndefined) -> String {
  case value {
    html.AriaBoolMixed(value) -> attr.bool_text(value)
    html.AriaMixed -> "mixed"
    html.AriaMixedUndefined -> "undefined"
  }
}

pub fn current(value: html.AriaCurrent) -> String {
  case value {
    html.AriaCurrentBool(value) -> attr.bool_text(value)
    html.AriaCurrentDate -> "date"
    html.AriaCurrentLocation -> "location"
    html.AriaCurrentPage -> "page"
    html.AriaCurrentStep -> "step"
    html.AriaCurrentTime -> "time"
  }
}

pub fn has_popup(value: html.AriaHasPopup) -> String {
  case value {
    html.AriaHasPopupBool(value) -> attr.bool_text(value)
    html.AriaHasPopupDialog -> "dialog"
    html.AriaHasPopupGrid -> "grid"
    html.AriaHasPopupListbox -> "listbox"
    html.AriaHasPopupMenu -> "menu"
    html.AriaHasPopupTree -> "tree"
  }
}

pub fn live(value: html.AriaLive) -> String {
  case value {
    html.AriaLiveAssertive -> "assertive"
    html.AriaLiveOff -> "off"
    html.AriaLivePolite -> "polite"
  }
}

pub fn orientation(value: html.AriaOrientation) -> String {
  case value {
    html.AriaOrientationHorizontal -> "horizontal"
    html.AriaOrientationUndefined -> "undefined"
    html.AriaOrientationVertical -> "vertical"
  }
}

pub fn sort(value: html.AriaSort) -> String {
  case value {
    html.AriaSortAscending -> "ascending"
    html.AriaSortDescending -> "descending"
    html.AriaSortNone -> "none"
    html.AriaSortOther -> "other"
  }
}
