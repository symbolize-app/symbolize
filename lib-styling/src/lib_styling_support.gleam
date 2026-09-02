import lib_styling_atom as atom
import lib_styling_context as context
import lib_styling_expr as expr
import lib_styling_typed_expr as typed_expr

pub opaque type SupportTerm {
  Code(atom.AtomOpt)
  And(List(SupportTerm))
  Not(SupportTerm)
  Or(List(SupportTerm))
}

pub fn code(value: atom.AtomOpt) -> SupportTerm {
  Code(value)
}

pub fn and(first: SupportTerm, rest: List(SupportTerm)) -> SupportTerm {
  And([first, ..rest])
}

pub fn not(first: SupportTerm, rest: List(SupportTerm)) -> SupportTerm {
  Not(and(first, rest))
}

pub fn or(first: SupportTerm, rest: List(SupportTerm)) -> SupportTerm {
  Or([first, ..rest])
}

pub fn match(
  condition: SupportTerm,
  value: expr.Expression,
) -> expr.Expression {
  expr.scoped_with(
    "supports:" <> key(condition),
    fn(styling) {
      let #(styling, condition) = compile_term(styling, condition)
      #(styling, "@supports " <> condition)
    },
    value,
  )
}

pub fn match_typed(
  condition: SupportTerm,
  value: typed_expr.Expression(value),
) -> typed_expr.Expression(value) {
  typed_expr.scoped_with(
    "supports:" <> key(condition),
    fn(styling) {
      let #(styling, condition) = compile_term(styling, condition)
      #(styling, "@supports " <> condition)
    },
    value,
  )
}

fn key(term: SupportTerm) -> String {
  case term {
    Code(value) -> "code:" <> atom_key(atom.entries(value))
    And(values) -> "and(" <> term_keys(values, ",") <> ")"
    Not(value) -> "not(" <> key(value) <> ")"
    Or(values) -> "or(" <> term_keys(values, ",") <> ")"
  }
}

fn term_keys(values: List(SupportTerm), separator: String) -> String {
  case values {
    [] -> ""
    [first, ..rest] -> join_term_keys(rest, separator, key(first))
  }
}

fn join_term_keys(
  values: List(SupportTerm),
  separator: String,
  output: String,
) -> String {
  case values {
    [] -> output
    [first, ..rest] ->
      join_term_keys(rest, separator, output <> separator <> key(first))
  }
}

fn atom_key(values: List(atom.Atom)) -> String {
  case values {
    [] -> ""
    [first, ..rest] ->
      atom.property(first)
      <> ":"
      <> expr.expression_key(atom.expression(first))
      <> ";"
      <> atom_key(rest)
  }
}

fn compile_term(
  styling: context.Context,
  term: SupportTerm,
) -> #(context.Context, String) {
  case term {
    Code(value) -> compile_code_atoms(styling, atom.entries(value), [])
    And(values) -> compile_terms(styling, values, " and ")
    Not(value) -> {
      let #(styling, value) = compile_term(styling, value)
      #(styling, "(not " <> value <> ")")
    }
    Or(values) -> compile_terms(styling, values, " or ")
  }
}

fn compile_code_atoms(
  styling: context.Context,
  values: List(atom.Atom),
  output: List(String),
) -> #(context.Context, String) {
  case values {
    [] -> {
      let values = reverse(output, [])
      case list_length(values) > 1 {
        True -> #(styling, "(" <> join_strings(values, " and ") <> ")")
        False -> #(styling, join_strings(values, " and "))
      }
    }
    [first, ..rest] -> {
      let #(styling, value) = atom.compile_code(styling, first)
      compile_code_atoms(styling, rest, [value, ..output])
    }
  }
}

fn compile_terms(
  styling: context.Context,
  values: List(SupportTerm),
  separator: String,
) -> #(context.Context, String) {
  compile_terms_list(styling, values, [], separator)
}

fn compile_terms_list(
  styling: context.Context,
  values: List(SupportTerm),
  output: List(String),
  separator: String,
) -> #(context.Context, String) {
  case values {
    [] -> {
      let values = reverse(output, [])
      case list_length(values) > 1 {
        True -> #(styling, "(" <> join_strings(values, separator) <> ")")
        False -> #(styling, join_strings(values, separator))
      }
    }
    [first, ..rest] -> {
      let #(styling, value) = compile_term(styling, first)
      compile_terms_list(styling, rest, [value, ..output], separator)
    }
  }
}

fn join_strings(values: List(String), separator: String) -> String {
  case values {
    [] -> ""
    [first, ..rest] -> join_nonempty(rest, separator, first)
  }
}

fn join_nonempty(
  values: List(String),
  separator: String,
  output: String,
) -> String {
  case values {
    [] -> output
    [first, ..rest] ->
      join_nonempty(rest, separator, output <> separator <> first)
  }
}

fn reverse(values: List(a), output: List(a)) -> List(a) {
  case values {
    [] -> output
    [first, ..rest] -> reverse(rest, [first, ..output])
  }
}

fn list_length(values: List(a)) -> Int {
  case values {
    [] -> 0
    [_, ..rest] -> 1 + list_length(rest)
  }
}
