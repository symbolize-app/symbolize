import lib_styling_atom as atom
import lib_styling_container as container
import lib_styling_content as content_dsl
import lib_styling_context as context
import lib_styling_data as data
import lib_styling_gradient as gradient
import lib_styling_media as media
import lib_styling_select as select
import lib_styling_support as support
import lib_styling_typed_expr as typed_expr
import lib_styling_var as var_dsl

pub type Context =
  context.Context

pub type Rule =
  context.Rule

pub type AtomOpt =
  atom.AtomOpt

pub type Atom =
  atom.Atom

pub type ContainerName =
  container.ContainerName

pub type ContainerTerm =
  container.ContainerTerm

pub type MediaTerm =
  media.MediaTerm

pub type SelectTerm =
  select.SelectTerm

pub type SupportTerm =
  support.SupportTerm

pub type Var(value) =
  var_dsl.Var(value)

pub type Expression(value) =
  typed_expr.Expression(value)

pub type Length =
  data.Length

pub type LengthUnit =
  data.LengthUnit

pub type Pct =
  data.Pct

pub type Angle =
  data.Angle

pub type AngleUnit =
  data.AngleUnit

pub type CssString =
  data.CssString

pub type Color =
  data.Color

pub type Gradient =
  gradient.Gradient

pub type Image =
  gradient.Gradient

pub type SvgPaint =
  data.SvgPaint

pub type ContentValue =
  content_dsl.Value

pub fn styling() -> Context {
  context.styling()
}

pub fn compile(
  styling: Context,
  value: AtomOpt,
) -> #(Context, List(Rule), List(String)) {
  atom.compile(styling, value)
}

pub fn before(value: AtomOpt) -> AtomOpt {
  atom.before(value)
}

pub fn after(value: AtomOpt) -> AtomOpt {
  atom.after(value)
}

pub fn length_value(value: Float, unit: LengthUnit) -> Length {
  data.length(value, unit)
}

pub fn length(value: Float, unit: LengthUnit) -> Expression(data.Length) {
  typed_expr.length(data.length(value, unit))
}

pub fn em(value: Float) -> Expression(data.Length) {
  typed_expr.length(data.em(value))
}

pub fn lh(value: Float) -> Expression(data.Length) {
  typed_expr.length(data.lh(value))
}

pub fn pt(value: Float) -> Expression(data.Length) {
  typed_expr.length(data.pt(value))
}

pub fn px(value: Float) -> Expression(data.Length) {
  typed_expr.length(data.px(value))
}

pub fn rem(value: Float) -> Expression(data.Length) {
  typed_expr.length(data.rem(value))
}

pub fn rlh(value: Float) -> Expression(data.Length) {
  typed_expr.length(data.rlh(value))
}

pub fn pct(value: Float) -> Expression(data.Pct) {
  typed_expr.pct(data.pct(value))
}

pub fn number(value: Float) -> Expression(Float) {
  typed_expr.number(value)
}

pub fn deg(value: Float) -> Expression(data.Angle) {
  typed_expr.angle(data.deg(value))
}

pub fn string_literal(value: String) -> Expression(data.CssString) {
  typed_expr.css_string(data.string_literal(value))
}

pub fn attr(name: String) -> Expression(data.CssString) {
  typed_expr.css_string(data.attr(name))
}

pub fn rgb(
  red: Expression(data.Pct),
  green: Expression(data.Pct),
  blue: Expression(data.Pct),
) -> Expression(data.Color) {
  typed_expr.rgb(red, green, blue)
}

pub fn rgb_alpha(
  red: Expression(data.Pct),
  green: Expression(data.Pct),
  blue: Expression(data.Pct),
  alpha: Expression(data.Pct),
) -> Expression(data.Color) {
  typed_expr.rgb_alpha(red, green, blue, alpha)
}

pub fn hsl(
  hue: Expression(data.Angle),
  saturation: Expression(data.Pct),
  lightness: Expression(data.Pct),
) -> Expression(data.Color) {
  typed_expr.hsl(hue, saturation, lightness)
}

pub fn hsl_alpha(
  hue: Expression(data.Angle),
  saturation: Expression(data.Pct),
  lightness: Expression(data.Pct),
  alpha: Expression(data.Pct),
) -> Expression(data.Color) {
  typed_expr.hsl_alpha(hue, saturation, lightness, alpha)
}

pub fn light_dark(
  light: Expression(data.Color),
  dark: Expression(data.Color),
) -> Expression(data.Color) {
  typed_expr.light_dark(light, dark)
}

pub fn keyword(value: String) -> Expression(value) {
  typed_expr.keyword(value)
}

pub fn add(
  first: Expression(value),
  rest: List(Expression(value)),
) -> Expression(value) {
  typed_expr.add(first, rest)
}

pub fn sub(
  first: Expression(value),
  rest: List(Expression(value)),
) -> Expression(value) {
  typed_expr.sub(first, rest)
}

pub fn mul(
  first: Expression(value),
  rest: List(Expression(Float)),
) -> Expression(value) {
  typed_expr.mul(first, rest)
}

pub fn div(
  first: Expression(value),
  rest: List(Expression(Float)),
) -> Expression(value) {
  typed_expr.div(first, rest)
}

pub fn min(
  first: Expression(value),
  rest: List(Expression(value)),
) -> Expression(value) {
  typed_expr.min(first, rest)
}

pub fn max(
  first: Expression(value),
  rest: List(Expression(value)),
) -> Expression(value) {
  typed_expr.max(first, rest)
}

pub fn clamp(
  lower: Expression(value),
  initial: Expression(value),
  upper: Expression(value),
) -> Expression(value) {
  typed_expr.clamp(lower, initial, upper)
}

pub fn c(
  first: Expression(value),
  rest: List(Expression(value)),
) -> Expression(value) {
  typed_expr.cascade(first, rest)
}

pub fn empty() -> AtomOpt {
  atom.Empty
}

pub fn atom(property: String, value: Expression(value)) -> AtomOpt {
  atom.atom(property, typed_expr.erase(value))
}

pub fn content(first: ContentValue, rest: List(ContentValue)) -> AtomOpt {
  content_dsl.content(first, rest)
}
