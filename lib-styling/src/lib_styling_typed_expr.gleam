import lib_styling_context as context
import lib_styling_data as data
import lib_styling_expr as raw

// This is the typed counterpart to the source Expression<Value> surface.
// The runtime compiler remains shared with lib_styling_expr; the type
// parameter exists at the construction boundary and is erased only when an
// expression becomes an untyped CSS atom.
pub opaque type Expression(value) {
  Expression(raw.Expression)
}

pub fn erase(value: Expression(value)) -> raw.Expression {
  let Expression(value) = value
  value
}

pub fn length(value: data.Length) -> Expression(data.Length) {
  Expression(raw.length(value))
}

pub fn pct(value: data.Pct) -> Expression(data.Pct) {
  Expression(raw.pct(value))
}

pub fn angle(value: data.Angle) -> Expression(data.Angle) {
  Expression(raw.angle(value))
}

pub fn number(value: Float) -> Expression(Float) {
  Expression(raw.number(value))
}

// A keyword is polymorphic; its surrounding property supplies the CSS value
// type, just as the source generic helper does.
pub fn keyword(value: String) -> Expression(value) {
  Expression(raw.keyword(value))
}

pub fn color(value: data.Color) -> Expression(data.Color) {
  Expression(raw.color(value))
}

pub fn css_string(value: data.CssString) -> Expression(data.CssString) {
  Expression(raw.css_string(value))
}

pub fn gradient(value: data.Gradient) -> Expression(data.Gradient) {
  Expression(raw.gradient(value))
}

// Gradient constructors have heterogeneous, property-specific arguments
// (angle, colour, length, and percentage). The source still produces one
// typed Expression<Gradient>; keep that one construction seam in the
// gradient DSL rather than weakening the common typed function constructor.
pub fn gradient_function(
  name: String,
  separator: String,
  values: List(raw.Expression),
) -> Expression(data.Gradient) {
  Expression(raw.function(name, separator, values))
}

pub fn paint(value: data.SvgPaint) -> Expression(data.SvgPaint) {
  Expression(raw.paint(value))
}

pub fn rgb(
  red: Expression(data.Pct),
  green: Expression(data.Pct),
  blue: Expression(data.Pct),
) -> Expression(data.Color) {
  Expression(raw.rgb(erase(red), erase(green), erase(blue)))
}

// CSS colors are valid SVG paints. This widened constructor preserves that
// source relationship at a property boundary such as `fill` without making
// the opaque expression type covariant by convention.
pub fn rgb_paint(
  red: Expression(data.Pct),
  green: Expression(data.Pct),
  blue: Expression(data.Pct),
) -> Expression(data.SvgPaint) {
  Expression(raw.rgb(erase(red), erase(green), erase(blue)))
}

pub fn rgb_alpha(
  red: Expression(data.Pct),
  green: Expression(data.Pct),
  blue: Expression(data.Pct),
  alpha: Expression(data.Pct),
) -> Expression(data.Color) {
  Expression(raw.rgb_alpha(erase(red), erase(green), erase(blue), erase(alpha)))
}

pub fn hsl(
  hue: Expression(data.Angle),
  saturation: Expression(data.Pct),
  lightness: Expression(data.Pct),
) -> Expression(data.Color) {
  Expression(raw.hsl(erase(hue), erase(saturation), erase(lightness)))
}

pub fn hsl_alpha(
  hue: Expression(data.Angle),
  saturation: Expression(data.Pct),
  lightness: Expression(data.Pct),
  alpha: Expression(data.Pct),
) -> Expression(data.Color) {
  Expression(raw.hsl_alpha(
    erase(hue),
    erase(saturation),
    erase(lightness),
    erase(alpha),
  ))
}

pub fn light_dark(
  light: Expression(data.Color),
  dark: Expression(data.Color),
) -> Expression(data.Color) {
  Expression(raw.light_dark(erase(light), erase(dark)))
}

pub fn calc(
  operator: String,
  values: List(Expression(value)),
) -> Expression(value) {
  Expression(raw.calc(operator, erase_list(values)))
}

pub fn add(
  first: Expression(value),
  rest: List(Expression(value)),
) -> Expression(value) {
  calc("+", [first, ..rest])
}

pub fn sub(
  first: Expression(value),
  rest: List(Expression(value)),
) -> Expression(value) {
  calc("-", [first, ..rest])
}

pub fn mul(
  first: Expression(value),
  rest: List(Expression(Float)),
) -> Expression(value) {
  Expression(raw.mul(erase(first), erase_list(rest)))
}

pub fn div(
  first: Expression(value),
  rest: List(Expression(Float)),
) -> Expression(value) {
  Expression(raw.div(erase(first), erase_list(rest)))
}

pub fn min(
  first: Expression(value),
  rest: List(Expression(value)),
) -> Expression(value) {
  Expression(raw.min(erase(first), erase_list(rest)))
}

pub fn max(
  first: Expression(value),
  rest: List(Expression(value)),
) -> Expression(value) {
  Expression(raw.max(erase(first), erase_list(rest)))
}

pub fn clamp(
  lower: Expression(value),
  initial: Expression(value),
  upper: Expression(value),
) -> Expression(value) {
  Expression(raw.clamp(erase(lower), erase(initial), erase(upper)))
}

pub fn function(
  name: String,
  separator: String,
  values: List(Expression(value)),
) -> Expression(value) {
  Expression(raw.function(name, separator, erase_list(values)))
}

pub fn joined(
  separator: String,
  values: List(Expression(value)),
) -> Expression(value) {
  Expression(raw.joined(separator, erase_list(values)))
}

pub fn cascade(
  first: Expression(value),
  rest: List(Expression(value)),
) -> Expression(value) {
  Expression(raw.cascade(erase(first), erase_list(rest)))
}

pub fn scoped(scope: String, body: Expression(value)) -> Expression(value) {
  Expression(raw.scoped(scope, erase(body)))
}

pub fn scoped_with(
  key: String,
  compile_scope: fn(context.Context) -> #(context.Context, String),
  body: Expression(value),
) -> Expression(value) {
  Expression(raw.scoped_with(key, compile_scope, erase(body)))
}

pub fn variable(variable: context.Variable) -> Expression(value) {
  Expression(raw.variable(variable))
}

pub fn variable_default(
  variable: context.Variable,
  default: Expression(value),
) -> Expression(value) {
  Expression(raw.variable_default(variable, erase(default)))
}

pub fn container(container: context.Container) -> Expression(value) {
  Expression(raw.container(container))
}

fn erase_list(values: List(Expression(value))) -> List(raw.Expression) {
  case values {
    [] -> []
    [first, ..rest] -> [erase(first), ..erase_list(rest)]
  }
}
