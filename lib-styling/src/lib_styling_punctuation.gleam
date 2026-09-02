import gleam/option
import lib_styling_atom as atom
import lib_styling_expr as expr
import lib_styling_typed_expr as typed_expr

pub type Value {
  First
  AllowEnd
  Last
  Expression(typed_expr.Expression(PunctuationExpressionValue))
}

pub type PunctuationExpressionValue {
  PunctuationExpressionValueMarker
}

// This mirrors the source's non-empty tuple union. The first populated slot
// determines the constructor, while later slots remain optional and ordered.
// There is deliberately no empty constructor: the source declaration tests
// reject `hang()` with no values.
pub type Values {
  ValuesFirst(Value, option.Option(Value), option.Option(Value))
  ValuesAllowEnd(Value, option.Option(Value))
  ValuesLast(Value)
}

pub type Hanging =
  option.Option(Values)

pub fn hang(value: Hanging) -> atom.AtomOpt {
  let expression = case value {
    option.None -> expr.keyword("none")
    option.Some(ValuesFirst(first, allow_end, last)) ->
      expr.function("", " ", [
        value_expression(first),
        ..optionals([allow_end, last])
      ])
    option.Some(ValuesAllowEnd(allow_end, last)) ->
      expr.function("", " ", [value_expression(allow_end), ..optionals([last])])
    option.Some(ValuesLast(last)) ->
      expr.function("", " ", [value_expression(last)])
  }
  atom.atom("hanging-punctuation", expression)
}

fn optionals(values: List(option.Option(Value))) -> List(expr.Expression) {
  case values {
    [] -> []
    [first, ..rest] -> append_optional(first, optionals(rest))
  }
}

fn append_optional(
  value: option.Option(Value),
  rest: List(expr.Expression),
) -> List(expr.Expression) {
  case value {
    option.None -> rest
    option.Some(value) -> [value_expression(value), ..rest]
  }
}

fn value_expression(value: Value) -> expr.Expression {
  case value {
    First -> expr.keyword("first")
    AllowEnd -> expr.keyword("allow-end")
    Last -> expr.keyword("last")
    Expression(value) -> typed_expr.erase(value)
  }
}
