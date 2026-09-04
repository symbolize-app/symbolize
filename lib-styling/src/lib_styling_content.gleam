import lib_styling_atom as atom
import lib_styling_data as data
import lib_styling_expr as expr
import lib_styling_typed_expr as typed_expr

pub type Value {
  String(data.CssString)
  Expression(typed_expr.Expression(data.CssString))
}

pub fn content(first: Value, rest: List(Value)) -> atom.AtomOpt {
  atom.atom(
    "content",
    expr.function("", " ", [value_expression(first), ..value_expressions(rest)]),
  )
}

fn value_expression(value: Value) -> expr.Expression {
  case value {
    String(value) -> expr.css_string(value)
    Expression(value) -> typed_expr.erase(value)
  }
}

fn value_expressions(values: List(Value)) -> List(expr.Expression) {
  case values {
    [] -> []
    [first, ..rest] -> [value_expression(first), ..value_expressions(rest)]
  }
}
