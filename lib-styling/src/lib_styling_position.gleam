import lib_styling_atom as atom
import lib_styling_expr as expr
import lib_styling_typed_expr as typed_expr

pub type Value {
  Absolute
  Fixed
  Relative
  Static
  Sticky
  Expression(typed_expr.Expression(PositionExpressionValue))
}

pub type PositionExpressionValue {
  PositionExpressionValueMarker
}

pub fn position(value: Value) -> atom.AtomOpt {
  atom.atom("position", value_expression(value))
}

fn value_expression(value: Value) -> expr.Expression {
  case value {
    Absolute -> expr.keyword("absolute")
    Fixed -> expr.keyword("fixed")
    Relative -> expr.keyword("relative")
    Static -> expr.keyword("static")
    Sticky -> expr.keyword("sticky")
    Expression(value) -> typed_expr.erase(value)
  }
}
