import lib_styling_atom as atom
import lib_styling_context as context
import lib_styling_expr as expr
import lib_styling_typed_expr as typed_expr
import lib_styling_values as values

pub opaque type Container {
  Container(context.Container)
}

// The source Container class implements Expression<ContainerName>. Keep that
// value family distinct from arbitrary CSS expressions so container.name
// cannot accidentally receive a colour, length, or keyword expression.
pub type ContainerName

pub opaque type ContainerTerm {
  ContainerTerm(
    key: String,
    compile: fn(context.Context) -> #(context.Context, String),
  )
}

pub type ContainerType {
  InlineSize
  Normal
  Size
}

pub type Orientation {
  Landscape
  Portrait
}

pub fn build(styling: context.Context) -> #(context.Context, Container) {
  let #(styling, container) = context.container(styling)
  #(styling, Container(container))
}

pub fn value(container: Container) -> typed_expr.Expression(ContainerName) {
  let Container(container) = container
  typed_expr.container(container)
}

pub fn name(
  value: typed_expr.Expression(ContainerName),
  rest: List(typed_expr.Expression(ContainerName)),
) -> atom.AtomOpt {
  atom.atom(
    "container-name",
    typed_expr.erase(typed_expr.joined(" ", [value, ..rest])),
  )
}

pub fn type_(value: ContainerType) -> atom.AtomOpt {
  atom.atom("container-type", expr.keyword(container_type_text(value)))
}

pub fn orientation(value: Orientation) -> ContainerTerm {
  feature("orientation", orientation_text(value))
}

pub fn min_width(value: values.LengthPercentage) -> ContainerTerm {
  expression_feature("min-width", values.length_percentage(value))
}

// Source namespace spelling: styling.container.min.w(...).
pub fn min_w(value: values.LengthPercentage) -> ContainerTerm {
  min_width(value)
}

pub fn min_height(value: values.LengthPercentage) -> ContainerTerm {
  expression_feature("min-height", values.length_percentage(value))
}

// Source namespace spelling: styling.container.min.h(...).
pub fn min_h(value: values.LengthPercentage) -> ContainerTerm {
  min_height(value)
}

pub fn min_block_size(value: values.LengthPercentage) -> ContainerTerm {
  expression_feature("min-block-size", values.length_percentage(value))
}

// Source namespace spelling: styling.container.min.o(...).
pub fn min_o(value: values.LengthPercentage) -> ContainerTerm {
  min_block_size(value)
}

pub fn min_inline_size(value: values.LengthPercentage) -> ContainerTerm {
  expression_feature("min-inline-size", values.length_percentage(value))
}

// Source namespace spelling: styling.container.min.i(...).
pub fn min_i(value: values.LengthPercentage) -> ContainerTerm {
  min_inline_size(value)
}

pub fn max_width(value: values.LengthPercentage) -> ContainerTerm {
  expression_feature("max-width", values.length_percentage(value))
}

// Source namespace spelling: styling.container.max.w(...).
pub fn max_w(value: values.LengthPercentage) -> ContainerTerm {
  max_width(value)
}

pub fn max_height(value: values.LengthPercentage) -> ContainerTerm {
  expression_feature("max-height", values.length_percentage(value))
}

// Source namespace spelling: styling.container.max.h(...).
pub fn max_h(value: values.LengthPercentage) -> ContainerTerm {
  max_height(value)
}

pub fn max_block_size(value: values.LengthPercentage) -> ContainerTerm {
  expression_feature("max-block-size", values.length_percentage(value))
}

// Source namespace spelling: styling.container.max.o(...).
pub fn max_o(value: values.LengthPercentage) -> ContainerTerm {
  max_block_size(value)
}

pub fn max_inline_size(value: values.LengthPercentage) -> ContainerTerm {
  expression_feature("max-inline-size", values.length_percentage(value))
}

// Source namespace spelling: styling.container.max.i(...).
pub fn max_i(value: values.LengthPercentage) -> ContainerTerm {
  max_inline_size(value)
}

pub fn and(first: ContainerTerm, rest: List(ContainerTerm)) -> ContainerTerm {
  let values = [first, ..rest]
  ContainerTerm("and(" <> join_keys(values, ",") <> ")", fn(styling) {
    compile_terms(styling, values, " and ")
  })
}

pub fn not(first: ContainerTerm, rest: List(ContainerTerm)) -> ContainerTerm {
  let term = and(first, rest)
  ContainerTerm("not(" <> key(term) <> ")", fn(styling) {
    let #(styling, value) = compile_term(styling, term)
    #(styling, "(not " <> value <> ")")
  })
}

pub fn or(first: ContainerTerm, rest: List(ContainerTerm)) -> ContainerTerm {
  let values = [first, ..rest]
  ContainerTerm("or(" <> join_keys(values, ",") <> ")", fn(styling) {
    compile_terms(styling, values, " or ")
  })
}

pub fn match_all(
  condition: ContainerTerm,
  value: expr.Expression,
) -> expr.Expression {
  expr.scoped_with(
    "container:all:" <> key(condition),
    fn(styling) {
      let #(styling, condition) = compile_term(styling, condition)
      #(styling, "@container " <> condition)
    },
    value,
  )
}

pub fn match_all_typed(
  condition: ContainerTerm,
  value: typed_expr.Expression(value),
) -> typed_expr.Expression(value) {
  typed_expr.scoped_with(
    "container:all:" <> key(condition),
    fn(styling) {
      let #(styling, condition) = compile_term(styling, condition)
      #(styling, "@container " <> condition)
    },
    value,
  )
}

pub fn match_named(
  container: Container,
  condition: ContainerTerm,
  value: expr.Expression,
) -> expr.Expression {
  let Container(container) = container
  expr.scoped_with(
    "container:" <> context.container_key(container) <> ":" <> key(condition),
    fn(styling) {
      let #(styling, name) = context.container_name(styling, container)
      let #(styling, condition) = compile_term(styling, condition)
      #(styling, "@container " <> name <> " " <> condition)
    },
    value,
  )
}

pub fn match_named_typed(
  container: Container,
  condition: ContainerTerm,
  value: typed_expr.Expression(value),
) -> typed_expr.Expression(value) {
  let Container(container) = container
  typed_expr.scoped_with(
    "container:" <> context.container_key(container) <> ":" <> key(condition),
    fn(styling) {
      let #(styling, name) = context.container_name(styling, container)
      let #(styling, condition) = compile_term(styling, condition)
      #(styling, "@container " <> name <> " " <> condition)
    },
    value,
  )
}

fn feature(name: String, value: String) -> ContainerTerm {
  ContainerTerm("feature:" <> name <> ":" <> value, fn(styling) {
    #(styling, "(" <> name <> ": " <> value <> ")")
  })
}

fn container_type_text(value: ContainerType) -> String {
  case value {
    InlineSize -> "inline-size"
    Normal -> "normal"
    Size -> "size"
  }
}

fn orientation_text(value: Orientation) -> String {
  case value {
    Landscape -> "landscape"
    Portrait -> "portrait"
  }
}

fn expression_feature(name: String, value: expr.Expression) -> ContainerTerm {
  ContainerTerm(
    "expression:" <> name <> ":" <> expr.expression_key(value),
    fn(styling) {
      let #(styling, value) = expr.compile_scope_pure(styling, value)
      #(styling, "(" <> name <> ": " <> value <> ")")
    },
  )
}

fn key(term: ContainerTerm) -> String {
  let ContainerTerm(key, _) = term
  key
}

fn compile_term(
  styling: context.Context,
  term: ContainerTerm,
) -> #(context.Context, String) {
  let ContainerTerm(_, compile) = term
  compile(styling)
}

fn compile_terms(
  styling: context.Context,
  terms: List(ContainerTerm),
  separator: String,
) -> #(context.Context, String) {
  let #(styling, values) = compile_terms_list(styling, terms, [])
  let value = join_strings(values, separator)
  case list_length(values) > 1 {
    True -> #(styling, "(" <> value <> ")")
    False -> #(styling, value)
  }
}

fn compile_terms_list(
  styling: context.Context,
  terms: List(ContainerTerm),
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

fn join_keys(values: List(ContainerTerm), separator: String) -> String {
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

fn reverse(values: List(a), output: List(a)) -> List(a) {
  case values {
    [] -> output
    [first, ..rest] -> reverse(rest, [first, ..output])
  }
}

fn list_length(values: List(a)) -> Int {
  case values {
    [] -> 0
    [_, ..rest] -> 1 + list_length(rest)
  }
}
