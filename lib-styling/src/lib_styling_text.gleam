import lib_styling_atom as atom
import lib_styling_expr as expr
import lib_styling_typed_expr as typed_expr

pub type Wrap {
  Balance
  Nowrap
  Pretty
  Stable
  Wrap
  Expression(typed_expr.Expression(TextWrapExpressionValue))
}

pub type TextWrapExpressionValue {
  TextWrapExpressionValueMarker
}

pub fn wrap(value: Wrap) -> atom.AtomOpt {
  atom.atom("text-wrap", wrap_expression(value))
}

fn wrap_expression(value: Wrap) -> expr.Expression {
  case value {
    Balance -> expr.keyword("balance")
    Nowrap -> expr.keyword("nowrap")
    Pretty -> expr.keyword("pretty")
    Stable -> expr.keyword("stable")
    Wrap -> expr.keyword("wrap")
    Expression(value) -> typed_expr.erase(value)
  }
}
