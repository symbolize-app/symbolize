import lib_styling_atom as atom
import lib_styling_expr as expr
import lib_styling_typed_expr as typed_expr

pub type Events {
  Auto
  None
  Expression(typed_expr.Expression(PointerEventsExpressionValue))
}

pub type PointerEventsExpressionValue {
  PointerEventsExpressionValueMarker
}

pub fn events(value: Events) -> atom.AtomOpt {
  atom.atom("pointer-events", events_expression(value))
}

fn events_expression(value: Events) -> expr.Expression {
  case value {
    Auto -> expr.keyword("auto")
    None -> expr.keyword("none")
    Expression(value) -> typed_expr.erase(value)
  }
}
