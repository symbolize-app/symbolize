import lib_styling_context as context
import lib_styling_data as data

pub type Value {
  LengthValue(data.Length)
  PercentageValue(data.Pct)
  AngleValue(data.Angle)
  NumberValue(Float)
  KeywordValue(String)
  ColorValue(data.Color)
  StringValue(data.CssString)
  GradientValue(data.Gradient)
  PaintValue(data.SvgPaint)
}

pub type Expression {
  Literal(Value)
  Function(String, String, List(Expression))
  Cascade(List(Expression))
  Scope(String, fn(context.Context) -> #(context.Context, String), Expression)
  Variable(context.Variable)
  VariableDefault(context.Variable, Expression)
  Container(context.Container)
}

pub fn length(value: data.Length) -> Expression {
  Literal(LengthValue(value))
}

pub fn pct(value: data.Pct) -> Expression {
  Literal(PercentageValue(value))
}

pub fn angle(value: data.Angle) -> Expression {
  Literal(AngleValue(value))
}

pub fn number(value: Float) -> Expression {
  Literal(NumberValue(value))
}

pub fn keyword(value: String) -> Expression {
  Literal(KeywordValue(value))
}

pub fn color(value: data.Color) -> Expression {
  Literal(ColorValue(value))
}

pub fn css_string(value: data.CssString) -> Expression {
  Literal(StringValue(value))
}

pub fn gradient(value: data.Gradient) -> Expression {
  Literal(GradientValue(value))
}

pub fn paint(value: data.SvgPaint) -> Expression {
  Literal(PaintValue(value))
}

pub fn rgb(red: Expression, green: Expression, blue: Expression) -> Expression {
  Function("rgb", " ", [red, green, blue])
}

pub fn rgb_alpha(
  red: Expression,
  green: Expression,
  blue: Expression,
  alpha: Expression,
) -> Expression {
  Function("rgb", " ", [red, green, blue, Literal(KeywordValue("/")), alpha])
}

pub fn hsl(
  hue: Expression,
  saturation: Expression,
  lightness: Expression,
) -> Expression {
  Function("hsl", " ", [hue, saturation, lightness])
}

pub fn hsl_alpha(
  hue: Expression,
  saturation: Expression,
  lightness: Expression,
  alpha: Expression,
) -> Expression {
  Function("hsl", " ", [
    hue,
    saturation,
    lightness,
    Literal(KeywordValue("/")),
    alpha,
  ])
}

pub fn light_dark(light: Expression, dark: Expression) -> Expression {
  Function("light-dark", ",", [light, dark])
}

pub fn calc(operator: String, values: List(Expression)) -> Expression {
  Function("calc", " " <> operator <> " ", values)
}

pub fn add(first: Expression, rest: List(Expression)) -> Expression {
  calc("+", [first, ..rest])
}

pub fn sub(first: Expression, rest: List(Expression)) -> Expression {
  calc("-", [first, ..rest])
}

pub fn mul(first: Expression, rest: List(Expression)) -> Expression {
  calc("*", [first, ..rest])
}

pub fn div(first: Expression, rest: List(Expression)) -> Expression {
  calc("/", [first, ..rest])
}

pub fn min(first: Expression, rest: List(Expression)) -> Expression {
  Function("min", ",", [first, ..rest])
}

pub fn max(first: Expression, rest: List(Expression)) -> Expression {
  Function("max", ",", [first, ..rest])
}

pub fn clamp(
  lower: Expression,
  initial: Expression,
  upper: Expression,
) -> Expression {
  Function("clamp", ",", [lower, initial, upper])
}

pub fn function(
  name: String,
  separator: String,
  values: List(Expression),
) -> Expression {
  Function(name, separator, values)
}

pub fn joined(separator: String, values: List(Expression)) -> Expression {
  Function("", separator, values)
}

pub fn cascade(first: Expression, rest: List(Expression)) -> Expression {
  Cascade([first, ..rest])
}

pub fn scoped(scope: String, body: Expression) -> Expression {
  scoped_with(scope, fn(context) { #(context, scope) }, body)
}

pub fn scoped_with(
  key: String,
  compile_scope: fn(context.Context) -> #(context.Context, String),
  body: Expression,
) -> Expression {
  Scope(key, compile_scope, body)
}

pub fn variable(variable: context.Variable) -> Expression {
  Variable(variable)
}

pub fn variable_default(
  variable: context.Variable,
  default: Expression,
) -> Expression {
  VariableDefault(variable, default)
}

pub fn container(container: context.Container) -> Expression {
  Container(container)
}

pub fn compile(
  context: context.Context,
  property: String,
  expression: Expression,
) -> #(context.Context, String, List(context.Rule)) {
  render_code(context, property, expression)
}

// Pseudo-elements must use the source's pure-expression path: scoped and
// cascading values become custom-property references, while their generated
// rules remain siblings of the pseudo-element rule.
pub fn compile_pure(
  context: context.Context,
  expression: Expression,
) -> #(context.Context, String, List(context.Rule)) {
  render_pure(context, expression)
}

pub fn compile_scope_pure(
  context: context.Context,
  expression: Expression,
) -> #(context.Context, String) {
  let #(context, value, rules) = render_pure(context, expression)
  case rules {
    [] -> #(context, value)
    _ -> panic as "Extra rules not allowed for scope conditions"
  }
}

pub fn expression_key(expression: Expression) -> String {
  case expression {
    Literal(value) -> "literal:" <> value_key(value)
    Function(name, separator, values) ->
      "function:" <> name <> ":" <> separator <> ":" <> list_key(values)
    Cascade(values) -> "cascade:" <> list_key(values)
    Scope(key, _, body) -> "scope:" <> key <> ":" <> expression_key(body)
    Variable(variable) -> "variable:" <> context.variable_key(variable)
    VariableDefault(variable, default) ->
      "variable-default:"
      <> context.variable_key(variable)
      <> ":"
      <> expression_key(default)
    Container(container) -> "container:" <> context.container_key(container)
  }
}

pub fn pure_text(expression: Expression) -> String {
  case expression {
    Literal(value) -> value_text(value)
    Variable(variable) -> "var(--s" <> context.variable_key(variable) <> ")"
    VariableDefault(variable, default) ->
      "var(--s"
      <> context.variable_key(variable)
      <> ", "
      <> pure_text(default)
      <> ")"
    Container(container) ->
      "var(--container-" <> context.container_key(container) <> ")"
    Function(name, separator, values) -> {
      let prefix = case name == "" {
        True -> ""
        False -> name <> "("
      }
      let suffix = case name == "" {
        True -> ""
        False -> ")"
      }
      prefix <> join_pure(values, separator) <> suffix
    }
    Scope(_, _, _) | Cascade(_) ->
      panic as "Extra rules not allowed for scope conditions"
  }
}

fn render_code(
  context: context.Context,
  property: String,
  expression: Expression,
) -> #(context.Context, String, List(context.Rule)) {
  case expression {
    Scope(_, compile_scope, body) -> {
      let #(context, scope) = compile_scope(context)
      let #(context, body_code, rules) = render_code(context, property, body)
      #(context, scope <> "{" <> body_code <> "}", rules)
    }
    Cascade(values) ->
      render_cascade(
        context,
        property,
        flatten_cascade(values),
        "",
        [],
        False,
        False,
      )
    _ -> {
      let #(context, value, rules) = render_pure(context, expression)
      #(context, property <> ":" <> value, rules)
    }
  }
}

fn render_pure(
  context: context.Context,
  expression: Expression,
) -> #(context.Context, String, List(context.Rule)) {
  case expression {
    Literal(value) -> #(context, value_text(value), [])
    Variable(variable) -> {
      let #(context, name) = context.variable_name(context, variable)
      #(context, "var(" <> name <> ")", [])
    }
    VariableDefault(id, default) -> {
      let #(context, name) = context.variable_name(context, id)
      let #(context, default_value, rules) = render_pure(context, default)
      #(context, "var(" <> name <> ", " <> default_value <> ")", rules)
    }
    Container(container) -> {
      let #(context, name) = context.container_name(context, container)
      #(context, name, [])
    }
    Function(name, separator, values) -> {
      let #(context, values, rules) =
        render_pure_values(context, values, [], [])
      let prefix = case name == "" {
        True -> ""
        False -> name <> "("
      }
      let suffix = case name == "" {
        True -> ""
        False -> ")"
      }
      #(context, prefix <> join(values, separator) <> suffix, rules)
    }
    Scope(_, _, _) | Cascade(_) -> extract_custom_property(context, expression)
  }
}

fn extract_custom_property(
  context: context.Context,
  expression: Expression,
) -> #(context.Context, String, List(context.Rule)) {
  let #(context, prepared_rules) =
    prepare_expression_children(context, expression)
  let key = expression_key(expression)
  let #(context, class_name, property_name, is_new) =
    context.expression_name(context, key)
  case is_new {
    False -> #(context, "var(" <> property_name <> ")", [])
    True -> {
      let #(context, code, rules) =
        render_code(context, property_name, expression)
      let rule =
        context.Rule(class_name, "." <> class_name <> "{" <> code <> "}")
      #(
        context,
        "var(" <> property_name <> ")",
        append(prepared_rules, append(rules, [rule])),
      )
    }
  }
}

// The source implementation compiles an ExpressionImpl's arguments before interning the
// expression itself. In particular, a pure function containing a cascade
// allocates custom properties for nested cascades first. The Gleam AST is
// intentionally simpler, so make that dependency order explicit at the one
// point where a cascade/scope becomes a custom property.
fn prepare_expression_children(
  context: context.Context,
  expression: Expression,
) -> #(context.Context, List(context.Rule)) {
  case expression {
    Literal(_) | Variable(_) | Container(_) -> #(context, [])
    VariableDefault(_, default) -> prepare_expression_children(context, default)
    Function(_, _, values) -> prepare_function_values(context, values)
    Cascade(values) -> prepare_cascade_values(context, values)
    Scope(_, _, body) -> prepare_expression_children(context, body)
  }
}

fn prepare_function_values(
  context: context.Context,
  values: List(Expression),
) -> #(context.Context, List(context.Rule)) {
  case values {
    [] -> #(context, [])
    [first, ..rest] -> {
      let #(context, rules) = prepare_expression_value(context, first)
      let #(context, rest_rules) = prepare_function_values(context, rest)
      #(context, append(rules, rest_rules))
    }
  }
}

fn prepare_expression_value(
  context: context.Context,
  expression: Expression,
) -> #(context.Context, List(context.Rule)) {
  case expression {
    Cascade(_) | Scope(_, _, _) -> {
      let #(context, _, rules) = render_pure(context, expression)
      #(context, rules)
    }
    _ -> prepare_expression_children(context, expression)
  }
}

fn prepare_cascade_values(
  context: context.Context,
  values: List(Expression),
) -> #(context.Context, List(context.Rule)) {
  case values {
    [] -> #(context, [])
    [first, ..rest] -> {
      let #(context, rules) = prepare_expression_children(context, first)
      let #(context, rest_rules) = prepare_cascade_values(context, rest)
      #(context, append(rules, rest_rules))
    }
  }
}

fn render_pure_values(
  context: context.Context,
  values: List(Expression),
  output: List(String),
  rules: List(context.Rule),
) -> #(context.Context, List(String), List(context.Rule)) {
  case values {
    [] -> #(context, output, rules)
    [first, ..rest] -> {
      let #(context, value, new_rules) = render_pure(context, first)
      render_pure_values(
        context,
        rest,
        append(output, [value]),
        append(rules, new_rules),
      )
    }
  }
}

fn render_cascade(
  context: context.Context,
  property: String,
  values: List(Expression),
  code: String,
  rules: List(context.Rule),
  scope_seen: Bool,
  semi: Bool,
) -> #(context.Context, String, List(context.Rule)) {
  case values {
    [] ->
      case scope_seen && semi {
        True -> #(context, code <> "}", rules)
        False -> #(context, code, rules)
      }
    [first, ..rest] ->
      case first {
        Scope(_, compile_scope, body) -> {
          let #(context, scope) = compile_scope(context)
          let #(context, inner, body_rules) =
            render_code(context, property, body)
          let separator = case semi {
            True -> ";"
            False -> ""
          }
          render_cascade(
            context,
            property,
            rest,
            code <> separator <> scope <> "{" <> inner <> "}",
            append(rules, body_rules),
            True,
            False,
          )
        }
        _ -> {
          let #(context, value, value_rules) = render_pure(context, first)
          let separator = case semi, scope_seen {
            True, _ -> ";"
            False, True -> "&{"
            False, False -> ""
          }
          render_cascade(
            context,
            property,
            rest,
            code <> separator <> property <> ":" <> value,
            append(rules, value_rules),
            scope_seen,
            True,
          )
        }
      }
  }
}

fn flatten_cascade(values: List(Expression)) -> List(Expression) {
  case values {
    [] -> []
    [Cascade(inner), ..rest] ->
      append(flatten_cascade(inner), flatten_cascade(rest))
    [first, ..rest] -> [first, ..flatten_cascade(rest)]
  }
}

fn value_text(value: Value) -> String {
  case value {
    LengthValue(value) -> data.length_text(value)
    PercentageValue(value) -> data.percentage_text(value)
    AngleValue(value) -> data.angle_text(value)
    NumberValue(value) -> number_text(value)
    KeywordValue(value) -> value
    ColorValue(value) -> data.color_text(value)
    StringValue(value) -> data.string_text(value)
    GradientValue(value) -> data.gradient_text(value)
    PaintValue(value) -> data.svg_paint_text(value)
  }
}

fn value_key(value: Value) -> String {
  value_text(value)
}

fn number_text(value: Float) -> String {
  data.number_text(value)
}

fn list_key(values: List(Expression)) -> String {
  case values {
    [] -> ""
    [first, ..rest] -> expression_key(first) <> ";" <> list_key(rest)
  }
}

fn join_pure(values: List(Expression), separator: String) -> String {
  case values {
    [] -> ""
    [first] -> pure_text(first)
    [first, ..rest] ->
      pure_text(first) <> separator <> join_pure(rest, separator)
  }
}

fn join(values: List(String), separator: String) -> String {
  case values {
    [] -> ""
    [first] -> first
    [first, ..rest] -> first <> separator <> join(rest, separator)
  }
}

fn append(first: List(a), second: List(a)) -> List(a) {
  case first {
    [] -> second
    [head, ..tail] -> [head, ..append(tail, second)]
  }
}
