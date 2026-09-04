import lib_styling_context as context
import lib_styling_expr as expr
import lib_styling_typed_expr as typed_expr
import lib_styling_values as values

pub opaque type MediaTerm {
  MediaTerm(
    key: String,
    compile: fn(context.Context) -> #(context.Context, String),
  )
}

pub type MediaType {
  All
  Print
  Screen
}

pub type HoverValue {
  Hover
  NoHover
}

pub type PointerValue {
  Coarse
  Fine
  NoPointer
}

pub type StylingPreference {
  Custom
  Less
  More
  NoPreference
}

pub type MotionPreference {
  NoMotionPreference
  Reduce
}

pub type ColorScheme {
  Dark
  Light
}

pub type Orientation {
  Landscape
  Portrait
}

pub fn hover() -> MediaTerm {
  feature("hover", "hover")
}

pub fn hover_value(value: HoverValue) -> MediaTerm {
  feature("hover", hover_value_text(value))
}

pub fn any_input_hover() -> MediaTerm {
  feature("any-hover", "hover")
}

pub fn any_input_hover_value(value: HoverValue) -> MediaTerm {
  feature("any-hover", hover_value_text(value))
}

pub fn any_input_pointer(value: PointerValue) -> MediaTerm {
  feature("any-pointer", pointer_value_text(value))
}

pub fn pointer(value: PointerValue) -> MediaTerm {
  feature("pointer", pointer_value_text(value))
}

pub fn reduced_motion() -> MediaTerm {
  feature("prefers-reduced-motion", "reduce")
}

pub fn reduced_motion_value(value: MotionPreference) -> MediaTerm {
  feature("prefers-reduced-motion", motion_preference_text(value))
}

pub fn hint_styling(value: StylingPreference) -> MediaTerm {
  feature("prefers-styling", styling_preference_text(value))
}

pub fn hint_color_scheme(value: ColorScheme) -> MediaTerm {
  feature("prefers-color-scheme", color_scheme_text(value))
}

pub fn orientation(value: Orientation) -> MediaTerm {
  feature("orientation", orientation_text(value))
}

pub fn min_width(value: values.LengthPercentage) -> MediaTerm {
  expression_feature("min-width", values.length_percentage(value))
}

// Source namespace spelling: styling.media.min.w(...).
pub fn min_w(value: values.LengthPercentage) -> MediaTerm {
  min_width(value)
}

pub fn min_height(value: values.LengthPercentage) -> MediaTerm {
  expression_feature("min-height", values.length_percentage(value))
}

// Source namespace spelling: styling.media.min.h(...).
pub fn min_h(value: values.LengthPercentage) -> MediaTerm {
  min_height(value)
}

pub fn max_width(value: values.LengthPercentage) -> MediaTerm {
  expression_feature("max-width", values.length_percentage(value))
}

// Source namespace spelling: styling.media.max.w(...).
pub fn max_w(value: values.LengthPercentage) -> MediaTerm {
  max_width(value)
}

pub fn max_height(value: values.LengthPercentage) -> MediaTerm {
  expression_feature("max-height", values.length_percentage(value))
}

// Source namespace spelling: styling.media.max.h(...).
pub fn max_h(value: values.LengthPercentage) -> MediaTerm {
  max_height(value)
}

pub fn and(first: MediaTerm, rest: List(MediaTerm)) -> MediaTerm {
  let values = [first, ..rest]
  MediaTerm("and(" <> join_keys(values, ",") <> ")", fn(styling) {
    compile_terms(styling, values, " and ", True)
  })
}

pub fn not(first: MediaTerm, rest: List(MediaTerm)) -> MediaTerm {
  let term = and(first, rest)
  MediaTerm("not(" <> key(term) <> ")", fn(styling) {
    let #(styling, value) = compile_term(styling, term)
    #(styling, "(not " <> value <> ")")
  })
}

pub fn or(first: MediaTerm, rest: List(MediaTerm)) -> MediaTerm {
  let values = [first, ..rest]
  MediaTerm("or(" <> join_keys(values, ",") <> ")", fn(styling) {
    compile_terms(styling, values, " or ", True)
  })
}

pub fn match(
  media_type: MediaType,
  condition: MediaTerm,
  value: expr.Expression,
) -> expr.Expression {
  expr.scoped_with(
    "media:" <> media_type_text(media_type) <> ":" <> key(condition),
    fn(styling) {
      let #(styling, condition) = compile_term(styling, condition)
      #(
        styling,
        "@media only " <> media_type_text(media_type) <> " and " <> condition,
      )
    },
    value,
  )
}

pub fn match_typed(
  media_type: MediaType,
  condition: MediaTerm,
  value: typed_expr.Expression(value),
) -> typed_expr.Expression(value) {
  typed_expr.scoped_with(
    "media:" <> media_type_text(media_type) <> ":" <> key(condition),
    fn(styling) {
      let #(styling, condition) = compile_term(styling, condition)
      #(
        styling,
        "@media only " <> media_type_text(media_type) <> " and " <> condition,
      )
    },
    value,
  )
}

fn media_type_text(value: MediaType) -> String {
  case value {
    All -> "all"
    Print -> "print"
    Screen -> "screen"
  }
}

fn hover_value_text(value: HoverValue) -> String {
  case value {
    Hover -> "hover"
    NoHover -> "none"
  }
}

fn pointer_value_text(value: PointerValue) -> String {
  case value {
    Coarse -> "coarse"
    Fine -> "fine"
    NoPointer -> "none"
  }
}

fn styling_preference_text(value: StylingPreference) -> String {
  case value {
    Custom -> "custom"
    Less -> "less"
    More -> "more"
    NoPreference -> "no-preference"
  }
}

fn motion_preference_text(value: MotionPreference) -> String {
  case value {
    NoMotionPreference -> "no-preference"
    Reduce -> "reduce"
  }
}

fn color_scheme_text(value: ColorScheme) -> String {
  case value {
    Dark -> "dark"
    Light -> "light"
  }
}

fn orientation_text(value: Orientation) -> String {
  case value {
    Landscape -> "landscape"
    Portrait -> "portrait"
  }
}

fn feature(name: String, value: String) -> MediaTerm {
  MediaTerm("feature:" <> name <> ":" <> value, fn(styling) {
    #(styling, "(" <> name <> ": " <> value <> ")")
  })
}

fn expression_feature(name: String, value: expr.Expression) -> MediaTerm {
  MediaTerm(
    "expression:" <> name <> ":" <> expr.expression_key(value),
    fn(styling) {
      let #(styling, value) = expr.compile_scope_pure(styling, value)
      #(styling, "(" <> name <> ": " <> value <> ")")
    },
  )
}

fn key(term: MediaTerm) -> String {
  let MediaTerm(key, _) = term
  key
}

fn compile_term(
  styling: context.Context,
  term: MediaTerm,
) -> #(context.Context, String) {
  let MediaTerm(_, compile) = term
  compile(styling)
}

fn compile_terms(
  styling: context.Context,
  terms: List(MediaTerm),
  separator: String,
  parenthesize: Bool,
) -> #(context.Context, String) {
  let #(styling, values) = compile_terms_list(styling, terms, [])
  let value = join_strings(values, separator)
  case parenthesize && list_length(values) > 1 {
    True -> #(styling, "(" <> value <> ")")
    False -> #(styling, value)
  }
}

fn compile_terms_list(
  styling: context.Context,
  terms: List(MediaTerm),
  values: List(String),
) -> #(context.Context, List(String)) {
  case terms {
    [] -> #(styling, reverse(values, []))
    [first, ..rest] -> {
      let #(styling, value) = compile_term(styling, first)
      compile_terms_list(styling, rest, [value, ..values])
    }
  }
}

fn reverse(values: List(a), output: List(a)) -> List(a) {
  case values {
    [] -> output
    [first, ..rest] -> reverse(rest, [first, ..output])
  }
}

fn join_keys(values: List(MediaTerm), separator: String) -> String {
  join_strings(list_map(values, key), separator)
}

fn list_map(values: List(a), transform: fn(a) -> b) -> List(b) {
  case values {
    [] -> []
    [first, ..rest] -> [transform(first), ..list_map(rest, transform)]
  }
}

fn join_strings(values: List(String), separator: String) -> String {
  case values {
    [] -> ""
    [first, ..rest] -> join_nonempty(rest, separator, first)
  }
}

fn join_nonempty(
  values: List(String),
  separator: String,
  output: String,
) -> String {
  case values {
    [] -> output
    [first, ..rest] ->
      join_nonempty(rest, separator, output <> separator <> first)
  }
}

fn list_length(values: List(a)) -> Int {
  case values {
    [] -> 0
    [_, ..rest] -> 1 + list_length(rest)
  }
}
