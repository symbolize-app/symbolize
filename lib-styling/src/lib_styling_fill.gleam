import lib_styling_atom as atom
import lib_styling_data as data
import lib_styling_typed_expr as typed_expr

pub type Value {
  Paint(data.SvgPaint)
  Expression(typed_expr.Expression(data.SvgPaint))
}

pub fn fill(value: Value) -> atom.AtomOpt {
  atom.atom("fill", typed_expr.erase(value_expression(value)))
}

fn value_expression(value: Value) -> typed_expr.Expression(data.SvgPaint) {
  case value {
    Paint(value) -> typed_expr.paint(value)
    Expression(value) -> value
  }
}
