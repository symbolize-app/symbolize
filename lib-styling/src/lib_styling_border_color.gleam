import lib_styling_atom as atom
import lib_styling_data as data
import lib_styling_typed_expr as typed_expr

pub type Value {
  Color(data.Color)
  Expression(typed_expr.Expression(data.Color))
}

pub fn t(value: Value) -> atom.AtomOpt {
  atom.atom("border-top-color", typed_expr.erase(value_expression(value)))
}

pub fn r(value: Value) -> atom.AtomOpt {
  atom.atom("border-right-color", typed_expr.erase(value_expression(value)))
}

pub fn b(value: Value) -> atom.AtomOpt {
  atom.atom("border-bottom-color", typed_expr.erase(value_expression(value)))
}

pub fn l(value: Value) -> atom.AtomOpt {
  atom.atom("border-left-color", typed_expr.erase(value_expression(value)))
}

pub fn tb(value: Value) -> atom.AtomOpt {
  atom.Many([t(value), b(value)])
}

pub fn rl(value: Value) -> atom.AtomOpt {
  atom.Many([r(value), l(value)])
}

pub fn trbl(value: Value) -> atom.AtomOpt {
  atom.Many([t(value), r(value), b(value), l(value)])
}

pub fn os(value: Value) -> atom.AtomOpt {
  atom.atom(
    "border-block-start-color",
    typed_expr.erase(value_expression(value)),
  )
}

pub fn oe(value: Value) -> atom.AtomOpt {
  atom.atom("border-block-end-color", typed_expr.erase(value_expression(value)))
}

pub fn is(value: Value) -> atom.AtomOpt {
  atom.atom(
    "border-inline-start-color",
    typed_expr.erase(value_expression(value)),
  )
}

pub fn ie(value: Value) -> atom.AtomOpt {
  atom.atom(
    "border-inline-end-color",
    typed_expr.erase(value_expression(value)),
  )
}

pub fn o(value: Value) -> atom.AtomOpt {
  atom.Many([os(value), oe(value)])
}

pub fn i(value: Value) -> atom.AtomOpt {
  atom.Many([is(value), ie(value)])
}

pub fn oi(value: Value) -> atom.AtomOpt {
  atom.Many([os(value), oe(value), is(value), ie(value)])
}

fn value_expression(value: Value) -> typed_expr.Expression(data.Color) {
  case value {
    Color(value) -> typed_expr.color(value)
    Expression(value) -> value
  }
}
