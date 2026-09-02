import gleam/list
import lib_styling_context as context
import lib_styling_expr as expr

pub type AtomOpt {
  Empty
  One(Atom)
  Many(List(AtomOpt))
}

pub type Pseudo {
  Base
  Before
  After
}

pub type Atom {
  Atom(Pseudo, Property, expr.Expression)
}

pub type Property {
  Named(String)
  Variable(context.Variable)
}

pub fn atom(property: String, value: expr.Expression) -> AtomOpt {
  One(Atom(Base, Named(property), value))
}

pub fn variable(property: context.Variable, value: expr.Expression) -> AtomOpt {
  One(Atom(Base, Variable(property), value))
}

pub fn before(value: AtomOpt) -> AtomOpt {
  map_pseudo(value, Before)
}

pub fn after(value: AtomOpt) -> AtomOpt {
  map_pseudo(value, After)
}

pub fn compile(
  context: context.Context,
  value: AtomOpt,
) -> #(context.Context, List(context.Rule), List(String)) {
  compile_atoms(context, final_atoms(flatten(value), []), [], [])
}

pub fn entries(value: AtomOpt) -> List(Atom) {
  flatten(value)
}

pub fn property(value: Atom) -> String {
  let Atom(_, property, _) = value
  case property {
    Named(property) -> property
    Variable(variable) -> context.variable_key(variable)
  }
}

pub fn expression(value: Atom) -> expr.Expression {
  let Atom(_, _, expression) = value
  expression
}

pub fn compile_code(
  context: context.Context,
  value: Atom,
) -> #(context.Context, String) {
  let Atom(_, property, expression) = value
  let #(context, expression) = expr.compile_scope_pure(context, expression)
  let #(context, property) = resolve_property(context, property)
  #(context, "(" <> property <> ":" <> expression <> ")")
}

fn compile_atoms(
  context: context.Context,
  atoms: List(Atom),
  rules: List(context.Rule),
  class_names: List(String),
) -> #(context.Context, List(context.Rule), List(String)) {
  case atoms {
    [] -> #(context, rules, class_names)
    [Atom(pseudo, property, value), ..rest] -> {
      let #(context, property_name) = resolve_property(context, property)
      let #(context, body, extra_rules) = case pseudo {
        Base -> expr.compile(context, property_name, value)
        _ -> {
          let #(context, value, rules) = expr.compile_pure(context, value)
          #(context, property_name <> ":" <> value, rules)
        }
      }
      let key =
        pseudo_text(pseudo)
        <> "|"
        <> property_key(property)
        <> "|"
        <> expr.expression_key(value)
      let #(context, class_name, is_new) = context.atom_name(context, key)
      case is_new {
        False -> {
          let cached_rules = case context.cached_atom_rules(context, key) {
            Ok(value) -> value
            Error(Nil) -> []
          }
          compile_atoms(
            context,
            rest,
            append(rules, cached_rules),
            append(class_names, rule_names(cached_rules)),
          )
        }
        True -> {
          let rule =
            context.Rule(
              class_name,
              "." <> class_name <> pseudo_text(pseudo) <> "{" <> body <> "}",
            )
          let atom_rules = append(extra_rules, [rule])
          let context = context.remember_atom_rules(context, key, atom_rules)
          compile_atoms(
            context,
            rest,
            append(rules, atom_rules),
            append(class_names, append(rule_names(extra_rules), [class_name])),
          )
        }
      }
    }
  }
}

fn rule_names(rules: List(context.Rule)) -> List(String) {
  case rules {
    [] -> []
    [first, ..rest] -> [context.rule_class_name(first), ..rule_names(rest)]
  }
}

fn flatten(value: AtomOpt) -> List(Atom) {
  case value {
    Empty -> []
    One(atom) -> [atom]
    Many(values) -> flatten_list(values)
  }
}

fn flatten_list(values: List(AtomOpt)) -> List(Atom) {
  case values {
    [] -> []
    [first, ..rest] -> append(flatten(first), flatten_list(rest))
  }
}

fn final_atoms(
  atoms: List(Atom),
  groups: List(#(Pseudo, List(Atom))),
) -> List(Atom) {
  case atoms {
    [] -> flatten_groups(groups)
    [first, ..rest] -> final_atoms(rest, group_upsert(first, groups))
  }
}

fn group_upsert(
  atom: Atom,
  groups: List(#(Pseudo, List(Atom))),
) -> List(#(Pseudo, List(Atom))) {
  let Atom(pseudo, ..) = atom
  case groups {
    [] -> [#(pseudo, [atom])]
    [#(group_pseudo, atoms), ..rest] ->
      case pseudo == group_pseudo {
        True -> [#(group_pseudo, atom_upsert(atom, atoms)), ..rest]
        False -> [#(group_pseudo, atoms), ..group_upsert(atom, rest)]
      }
  }
}

fn atom_upsert(atom: Atom, selected: List(Atom)) -> List(Atom) {
  case selected {
    [] -> [atom]
    [first, ..rest] ->
      case same_key(atom, first) {
        True -> [atom, ..rest]
        False -> [first, ..atom_upsert(atom, rest)]
      }
  }
}

fn flatten_groups(groups: List(#(Pseudo, List(Atom)))) -> List(Atom) {
  case groups {
    [] -> []
    [#(_, atoms), ..rest] -> append(atoms, flatten_groups(rest))
  }
}

fn same_key(left: Atom, right: Atom) -> Bool {
  let Atom(left_pseudo, left_property, _) = left
  let Atom(right_pseudo, right_property, _) = right
  left_pseudo == right_pseudo && left_property == right_property
}

fn property_key(property: Property) -> String {
  case property {
    Named(property) -> "name:" <> property
    Variable(variable) -> "variable:" <> context.variable_key(variable)
  }
}

fn property_text(property: Property) -> String {
  case property {
    Named(property) -> property
    Variable(variable) -> context.variable_key(variable)
  }
}

fn resolve_property(
  context: context.Context,
  property: Property,
) -> #(context.Context, String) {
  case property {
    Named(property) -> #(context, property)
    Variable(variable) -> context.variable_name(context, variable)
  }
}

fn map_pseudo(value: AtomOpt, pseudo: Pseudo) -> AtomOpt {
  case value {
    Empty -> Empty
    One(Atom(current, property, expression)) ->
      case current {
        Base -> One(Atom(pseudo, property, expression))
        _ -> {
          let message =
            property_text(property)
            <> " atom already has "
            <> pseudo_text(current)
            <> " pseudo-element"
          panic as message
        }
      }
    Many(values) ->
      Many(list.map(values, fn(item) { map_pseudo(item, pseudo) }))
  }
}

fn pseudo_text(pseudo: Pseudo) -> String {
  case pseudo {
    Base -> ""
    Before -> "::before"
    After -> "::after"
  }
}

fn append(first: List(a), second: List(a)) -> List(a) {
  case first {
    [] -> second
    [head, ..tail] -> [head, ..append(tail, second)]
  }
}
