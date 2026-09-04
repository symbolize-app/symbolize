import gleam/list
import gleam/option.{type Option, None, Some}
import lib_styling_atom as atom
import lib_styling_data as data
import lib_styling_expr as expr
import lib_styling_typed_expr as typed_expr

// The source Gradient is an Expression<Gradient>, not a separate rendered
// string. Sharing the typed expression representation lets gradients compose
// with the source's cascade/scope operations before a property erases them.
pub type Gradient =
  typed_expr.Expression(data.Gradient)

pub type AngleValue {
  Angle(data.Angle)
  AngleExpression(typed_expr.Expression(data.Angle))
}

pub type ColorValue {
  Color(data.Color)
  ColorExpression(typed_expr.Expression(data.Color))
}

pub type Position {
  Length(data.Length)
  Percentage(data.Pct)
  LengthExpression(typed_expr.Expression(data.Length))
  PercentageExpression(typed_expr.Expression(data.Pct))
}

pub type Stop {
  ColorStop(ColorValue)
  Positioned(ColorValue, Position, Option(Position))
}

pub type HintStop {
  Plain(Stop)
  WithHint(Position, Stop)
}

pub fn linear(
  angle: AngleValue,
  first_stop: Stop,
  rest_stops: List(HintStop),
) -> Gradient {
  linear_stops(angle, first_stop, rest_stops)
}

pub fn linear_stops(
  angle: AngleValue,
  first_stop: Stop,
  rest_stops: List(HintStop),
) -> Gradient {
  typed_expr.gradient_function("linear-gradient", ",", [
    angle_expression(angle),
    stop_expression(first_stop),
    ..hint_stop_expressions(rest_stops)
  ])
}

pub fn value(value: ColorValue) -> Stop {
  ColorStop(value)
}

pub fn stop(
  value: ColorValue,
  first_position: Position,
  second_position: Option(Position),
) -> Stop {
  Positioned(value, first_position, second_position)
}

pub fn plain(value: Stop) -> HintStop {
  Plain(value)
}

pub fn hint(position: Position, value: Stop) -> HintStop {
  WithHint(position, value)
}

pub fn background_image(first: Gradient, rest: List(Gradient)) -> atom.AtomOpt {
  atom.atom(
    "background-image",
    expr.joined(",", [to_expression(first), ..to_expressions(rest)]),
  )
}

pub fn to_expression(value: Gradient) -> expr.Expression {
  typed_expr.erase(value)
}

fn angle_expression(value: AngleValue) -> expr.Expression {
  case value {
    Angle(value) -> expr.angle(value)
    AngleExpression(value) -> typed_expr.erase(value)
  }
}

fn color_expression(value: ColorValue) -> expr.Expression {
  case value {
    Color(value) -> expr.color(value)
    ColorExpression(value) -> typed_expr.erase(value)
  }
}

fn position_expression(value: Position) -> expr.Expression {
  case value {
    Length(value) -> expr.length(value)
    Percentage(value) -> expr.pct(value)
    LengthExpression(value) -> typed_expr.erase(value)
    PercentageExpression(value) -> typed_expr.erase(value)
  }
}

fn stop_expression(value: Stop) -> expr.Expression {
  case value {
    ColorStop(value) -> color_expression(value)
    Positioned(value, first_position, second_position) ->
      expr.joined(
        " ",
        positioned_values(value, first_position, second_position),
      )
  }
}

fn positioned_values(
  value: ColorValue,
  first_position: Position,
  second_position: Option(Position),
) -> List(expr.Expression) {
  [
    color_expression(value),
    position_expression(first_position),
    ..optional_position(second_position)
  ]
}

fn optional_position(value: Option(Position)) -> List(expr.Expression) {
  case value {
    None -> []
    Some(value) -> [position_expression(value)]
  }
}

fn hint_stop_values(value: HintStop) -> List(expr.Expression) {
  case value {
    Plain(value) -> [stop_expression(value)]
    WithHint(position, value) -> [
      position_expression(position),
      stop_expression(value),
    ]
  }
}

fn hint_stop_expressions(values: List(HintStop)) -> List(expr.Expression) {
  case values {
    [] -> []
    [first, ..rest] ->
      list.append(hint_stop_values(first), hint_stop_expressions(rest))
  }
}

fn to_expressions(values: List(Gradient)) -> List(expr.Expression) {
  case values {
    [] -> []
    [first, ..rest] -> [to_expression(first), ..to_expressions(rest)]
  }
}
