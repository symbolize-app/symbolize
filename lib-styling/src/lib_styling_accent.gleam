import lib_styling_atom as atom
import lib_styling_data as data
import lib_styling_typed_expr as typed_expr

pub type Value {
  Auto
  Color(data.Color)
  Expression(typed_expr.Expression(data.Color))
}

pub fn color(value: Value) -> atom.AtomOpt {
  atom.atom("accent-color", typed_expr.erase(value_expression(value)))
}

fn value_expression(value: Value) -> typed_expr.Expression(data.Color) {
  case value {
    Auto -> typed_expr.keyword("auto")
    Color(value) -> typed_expr.color(value)
    Expression(value) -> value
  }
}
