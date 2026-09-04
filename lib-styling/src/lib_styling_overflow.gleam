import lib_styling_atom as atom
import lib_styling_expr as expr
import lib_styling_typed_expr as typed_expr

pub type Value {
  Auto
  Clip
  Hidden
  Scroll
  Visible
  Expression(typed_expr.Expression(OverflowExpressionValue))
}

pub type OverflowExpressionValue {
  OverflowExpressionValueMarker
}

pub type WrapValue {
  Anywhere
  BreakWord
  Normal
  WrapExpression(typed_expr.Expression(OverflowWrapExpressionValue))
}

pub type OverflowWrapExpressionValue {
  OverflowWrapExpressionValueMarker
}

pub fn x(value: Value) -> atom.AtomOpt {
  atom.atom("overflow-x", value_expression(value))
}

pub fn y(value: Value) -> atom.AtomOpt {
  atom.atom("overflow-y", value_expression(value))
}

pub fn xy(value: Value) -> atom.AtomOpt {
  atom.Many([x(value), y(value)])
}

pub fn wrap(value: WrapValue) -> atom.AtomOpt {
  atom.atom("overflow-wrap", wrap_expression(value))
}

fn value_expression(value: Value) -> expr.Expression {
  case value {
    Auto -> expr.keyword("auto")
    Clip -> expr.keyword("clip")
    Hidden -> expr.keyword("hidden")
    Scroll -> expr.keyword("scroll")
    Visible -> expr.keyword("visible")
    Expression(value) -> typed_expr.erase(value)
  }
}

fn wrap_expression(value: WrapValue) -> expr.Expression {
  case value {
    Anywhere -> expr.keyword("anywhere")
    BreakWord -> expr.keyword("break-word")
    Normal -> expr.keyword("normal")
    WrapExpression(value) -> typed_expr.erase(value)
  }
}
