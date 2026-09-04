import lib_styling_expr as expr
import lib_styling_typed_expr as typed_expr

pub opaque type SelectTerm {
  SelectTerm(String)
}

// This is the source select.dir(value: "ltr" | "rtl") union. Keeping the
// finite CSS vocabulary typed prevents callers from creating invalid :dir()
// selectors while preserving the source's emitted spelling.
pub type Direction {
  Ltr
  Rtl
}

pub fn raw(value: String) -> SelectTerm {
  SelectTerm(value)
}

pub fn hover() -> SelectTerm {
  state("hover")
}

pub fn disabled() -> SelectTerm {
  state("disabled")
}

pub fn empty() -> SelectTerm {
  state("empty")
}

pub fn first_child() -> SelectTerm {
  state("first-child")
}

pub fn last_child() -> SelectTerm {
  state("last-child")
}

pub fn and(first: SelectTerm, rest: List(SelectTerm)) -> SelectTerm {
  SelectTerm(join([first, ..rest], ""))
}

pub fn not(first: SelectTerm, rest: List(SelectTerm)) -> SelectTerm {
  let term = and(first, rest)
  SelectTerm(":not(" <> text(term) <> ")")
}

pub fn or(first: SelectTerm, rest: List(SelectTerm)) -> SelectTerm {
  SelectTerm(":where(" <> join([first, ..rest], ",") <> ")")
}

pub fn dir(value: Direction) -> SelectTerm {
  function("dir", [direction_text(value)])
}

pub fn lang(first: String, rest: List(String)) -> SelectTerm {
  function("lang", [first, ..rest])
}

pub fn match(condition: SelectTerm, value: expr.Expression) -> expr.Expression {
  expr.scoped("&:where(" <> text(condition) <> ")", value)
}

pub fn match_typed(
  condition: SelectTerm,
  value: typed_expr.Expression(value),
) -> typed_expr.Expression(value) {
  typed_expr.scoped("&:where(" <> text(condition) <> ")", value)
}

fn state(name: String) -> SelectTerm {
  SelectTerm(":" <> name)
}

fn function(name: String, values: List(String)) -> SelectTerm {
  SelectTerm(":" <> name <> "(" <> join_strings(values, ",") <> ")")
}

fn direction_text(value: Direction) -> String {
  case value {
    Ltr -> "ltr"
    Rtl -> "rtl"
  }
}

fn text(term: SelectTerm) -> String {
  let SelectTerm(value) = term
  value
}

fn join(terms: List(SelectTerm), separator: String) -> String {
  case terms {
    [] -> ""
    [first] -> text(first)
    [first, ..rest] -> text(first) <> separator <> join(rest, separator)
  }
}

fn join_strings(values: List(String), separator: String) -> String {
  case values {
    [] -> ""
    [first] -> first
    [first, ..rest] -> first <> separator <> join_strings(rest, separator)
  }
}
