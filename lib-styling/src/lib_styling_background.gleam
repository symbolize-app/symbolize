import lib_styling_atom as atom
import lib_styling_data as data
import lib_styling_expr as expr
import lib_styling_gradient as gradient
import lib_styling_typed_expr as typed_expr

pub type BackgroundSizeAutoValue {
  BackgroundSizeAutoValueMarker
}

pub type SingleSizeValue {
  Contain
  Cover
  Expression(typed_expr.Expression(BackgroundSizeKeywordValue))
}

pub type BackgroundSizeKeywordValue {
  BackgroundSizeKeywordValueMarker
}

pub type PairSizeValue {
  Auto
  Length(data.Length)
  Percentage(data.Pct)
  LengthExpression(typed_expr.Expression(data.Length))
  PercentageExpression(typed_expr.Expression(data.Pct))
  AutoExpression(typed_expr.Expression(BackgroundSizeAutoValue))
}

pub type Size {
  Single(SingleSizeValue)
  Pair(PairSizeValue, PairSizeValue)
}

pub type ColorValue {
  Color(data.Color)
  ColorExpression(typed_expr.Expression(data.Color))
}

pub fn color(value: ColorValue) -> atom.AtomOpt {
  atom.atom("background-color", color_expression(value))
}

// This separate name is retained as a convenient common-type entry point for
// callers that already hold a typed color expression.
pub fn color_typed(value: typed_expr.Expression(data.Color)) -> atom.AtomOpt {
  atom.atom("background-color", typed_expr.erase(value))
}

pub fn image(
  first: gradient.Gradient,
  rest: List(gradient.Gradient),
) -> atom.AtomOpt {
  atom.atom(
    "background-image",
    expr.joined(",", [
      gradient.to_expression(first),
      ..gradient_expressions(rest)
    ]),
  )
}

pub fn size(first: Size, rest: List(Size)) -> atom.AtomOpt {
  atom.atom(
    "background-size",
    expr.joined(",", [to_expression(first), ..to_expressions(rest)]),
  )
}

fn to_expression(value: Size) -> expr.Expression {
  case value {
    Single(value) -> single_size_expression(value)
    Pair(first, second) ->
      expr.joined(" ", [
        pair_size_expression(first),
        pair_size_expression(second),
      ])
  }
}

fn single_size_expression(value: SingleSizeValue) -> expr.Expression {
  case value {
    Contain -> expr.keyword("contain")
    Cover -> expr.keyword("cover")
    Expression(value) -> typed_expr.erase(value)
  }
}

fn pair_size_expression(value: PairSizeValue) -> expr.Expression {
  case value {
    Auto -> expr.keyword("auto")
    Length(value) -> expr.length(value)
    Percentage(value) -> expr.pct(value)
    LengthExpression(value) -> typed_expr.erase(value)
    PercentageExpression(value) -> typed_expr.erase(value)
    AutoExpression(value) -> typed_expr.erase(value)
  }
}

fn color_expression(value: ColorValue) -> expr.Expression {
  case value {
    Color(value) -> expr.color(value)
    ColorExpression(value) -> typed_expr.erase(value)
  }
}

fn gradient_expressions(
  values: List(gradient.Gradient),
) -> List(expr.Expression) {
  case values {
    [] -> []
    [first, ..rest] -> [
      gradient.to_expression(first),
      ..gradient_expressions(rest)
    ]
  }
}

fn to_expressions(values: List(Size)) -> List(expr.Expression) {
  case values {
    [] -> []
    [first, ..rest] -> [to_expression(first), ..to_expressions(rest)]
  }
}
