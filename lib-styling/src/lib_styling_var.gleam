import lib_styling_atom as atom
import lib_styling_context as context
import lib_styling_expr as expr
import lib_styling_typed_expr as typed_expr

pub opaque type Var(value) {
  Var(context.Variable)
}

pub fn build(styling: context.Context) -> #(context.Context, Var(value)) {
  let #(styling, variable) = context.variable(styling)
  #(styling, Var(variable))
}

pub fn resolve(
  styling: context.Context,
  variable: Var(value),
) -> #(context.Context, String) {
  let Var(variable) = variable
  context.variable_name(styling, variable)
}

pub fn get(variable: Var(value)) -> expr.Expression {
  let Var(variable) = variable
  expr.variable(variable)
}

pub fn get_typed(variable: Var(value)) -> typed_expr.Expression(value) {
  let Var(variable) = variable
  typed_expr.variable(variable)
}

pub fn or(variable: Var(value), default: expr.Expression) -> expr.Expression {
  let Var(variable) = variable
  expr.variable_default(variable, default)
}

pub fn or_typed(
  variable: Var(value),
  default: typed_expr.Expression(value),
) -> typed_expr.Expression(value) {
  let Var(variable) = variable
  typed_expr.variable_default(variable, default)
}

pub fn set(variable: Var(value), value: expr.Expression) -> atom.AtomOpt {
  let Var(variable) = variable
  atom.variable(variable, value)
}

pub fn set_typed(
  variable: Var(value),
  expression: typed_expr.Expression(value),
) -> atom.AtomOpt {
  let Var(variable) = variable
  atom.variable(variable, typed_expr.erase(expression))
}
