import lib_styling_atom as atom
import lib_styling_expr as expr
import lib_styling_typed_expr as typed_expr

pub type Value {
  Auto
  Isolate
  Expression(typed_expr.Expression(IsolationExpressionValue))
}

pub type IsolationExpressionValue {
  IsolationExpressionValueMarker
}

pub fn isolation(value: Value) -> atom.AtomOpt {
  atom.atom("isolation", value_expression(value))
}

fn value_expression(value: Value) -> expr.Expression {
  case value {
    Auto -> expr.keyword("auto")
    Isolate -> expr.keyword("isolate")
    Expression(value) -> typed_expr.erase(value)
  }
}
