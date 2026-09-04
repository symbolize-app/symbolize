import lib_styling_atom as atom
import lib_styling_expr as expr
import lib_styling_typed_expr as typed_expr

pub type Mode {
  Auto
  Manual
  None
  Expression(typed_expr.Expression(HyphensExpressionValue))
}

pub type HyphensExpressionValue {
  HyphensExpressionValueMarker
}

pub fn mode(value: Mode) -> atom.AtomOpt {
  atom.atom("hyphens", mode_expression(value))
}

fn mode_expression(value: Mode) -> expr.Expression {
  case value {
    Auto -> expr.keyword("auto")
    Manual -> expr.keyword("manual")
    None -> expr.keyword("none")
    Expression(value) -> typed_expr.erase(value)
  }
}
