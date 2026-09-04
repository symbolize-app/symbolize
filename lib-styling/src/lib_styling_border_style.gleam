import lib_styling_atom as atom
import lib_styling_typed_expr as typed_expr

pub type Style {
  Dashed
  Dotted
  Double
  Groove
  Hidden
  Inset
  None
  Outset
  Ridge
  Solid
  Expression(typed_expr.Expression(Style))
}

pub fn t(value: Style) -> atom.AtomOpt {
  atom.atom("border-top-style", typed_expr.erase(style_expression(value)))
}

pub fn r(value: Style) -> atom.AtomOpt {
  atom.atom("border-right-style", typed_expr.erase(style_expression(value)))
}

pub fn b(value: Style) -> atom.AtomOpt {
  atom.atom("border-bottom-style", typed_expr.erase(style_expression(value)))
}

pub fn l(value: Style) -> atom.AtomOpt {
  atom.atom("border-left-style", typed_expr.erase(style_expression(value)))
}

pub fn tb(value: Style) -> atom.AtomOpt {
  atom.Many([t(value), b(value)])
}

pub fn rl(value: Style) -> atom.AtomOpt {
  atom.Many([r(value), l(value)])
}

pub fn trbl(value: Style) -> atom.AtomOpt {
  atom.Many([t(value), r(value), b(value), l(value)])
}

pub fn os(value: Style) -> atom.AtomOpt {
  atom.atom(
    "border-block-start-style",
    typed_expr.erase(style_expression(value)),
  )
}

pub fn oe(value: Style) -> atom.AtomOpt {
  atom.atom("border-block-end-style", typed_expr.erase(style_expression(value)))
}

pub fn is(value: Style) -> atom.AtomOpt {
  atom.atom(
    "border-inline-start-style",
    typed_expr.erase(style_expression(value)),
  )
}

pub fn ie(value: Style) -> atom.AtomOpt {
  atom.atom(
    "border-inline-end-style",
    typed_expr.erase(style_expression(value)),
  )
}

pub fn o(value: Style) -> atom.AtomOpt {
  atom.Many([os(value), oe(value)])
}

pub fn i(value: Style) -> atom.AtomOpt {
  atom.Many([is(value), ie(value)])
}

pub fn oi(value: Style) -> atom.AtomOpt {
  atom.Many([os(value), oe(value), is(value), ie(value)])
}

fn style_expression(value: Style) -> typed_expr.Expression(Style) {
  case value {
    Dashed -> typed_expr.keyword("dashed")
    Dotted -> typed_expr.keyword("dotted")
    Double -> typed_expr.keyword("double")
    Groove -> typed_expr.keyword("groove")
    Hidden -> typed_expr.keyword("hidden")
    Inset -> typed_expr.keyword("inset")
    None -> typed_expr.keyword("none")
    Outset -> typed_expr.keyword("outset")
    Ridge -> typed_expr.keyword("ridge")
    Solid -> typed_expr.keyword("solid")
    Expression(value) -> value
  }
}
