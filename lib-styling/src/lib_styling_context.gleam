import gleam/int
import gleam/option.{type Option, None, Some}
import gleam/string
import lib_dataflow as dataflow

type Identity

@external(javascript, "./lib_styling_identity_ffi.mjs", "new_identity")
fn new_identity() -> Identity

@external(javascript, "./lib_styling_identity_ffi.mjs", "same_identity")
fn same_identity(first: Identity, second: Identity) -> Bool

pub type Rule {
  Rule(class_name: String, code: String)
}

pub opaque type Variable {
  Variable(Identity, Int)
}

pub opaque type Container {
  Container(Identity, Int)
}

type IdentitySource {
  IdentitySource(
    context: dataflow.Context,
    counter: dataflow.Mutation(#(Int, Int)),
  )
}

pub fn rule_class_name(rule: Rule) -> String {
  let Rule(class_name, _) = rule
  class_name
}

pub fn rule_code(rule: Rule) -> String {
  let Rule(_, code) = rule
  code
}

pub opaque type Context {
  Context(
    next_atom: Int,
    next_expression: Int,
    next_variable_token: Int,
    next_variable_name: Int,
    next_container_token: Int,
    next_container_name: Int,
    atom_names: List(#(String, String)),
    atom_rules: List(#(String, List(Rule))),
    expression_names: List(#(String, #(String, String))),
    variable_names: List(#(Variable, String)),
    container_names: List(#(Container, String)),
    identity_source: IdentitySource,
  )
}

pub fn styling() -> Context {
  Context(
    0,
    0,
    0,
    0,
    0,
    0,
    [],
    [],
    [],
    [],
    [],
    IdentitySource(dataflow.dataflow(), dataflow.state(#(0, 0))),
  )
}

pub fn variable(context: Context) -> #(Context, Variable) {
  let Context(
    atom,
    expression,
    next_variable_token,
    next_variable_name,
    next_container_token,
    next_container_name,
    atoms,
    rules,
    expressions,
    variable_names,
    container_names,
    identity_source,
  ) = context
  let token = take_variable_token(identity_source)
  #(
    Context(
      atom,
      expression,
      next_variable_token + 1,
      next_variable_name,
      next_container_token,
      next_container_name,
      atoms,
      rules,
      expressions,
      variable_names,
      container_names,
      identity_source,
    ),
    Variable(new_identity(), token),
  )
}

pub fn variable_key(variable: Variable) -> String {
  let Variable(_, token) = variable
  "v" <> int.to_string(token)
}

pub fn variable_name(
  context: Context,
  variable: Variable,
) -> #(Context, String) {
  let Context(
    atom,
    expression,
    next_variable_token,
    next_variable_name,
    next_container_token,
    next_container_name,
    atoms,
    rules,
    expressions,
    variable_names,
    container_names,
    identity_source,
  ) = context
  case find_variable(variable_names, variable) {
    Some(name) -> #(context, name)
    None -> {
      let name = "--s" <> int_text(next_variable_name)
      #(
        Context(
          atom,
          expression,
          next_variable_token,
          next_variable_name + 1,
          next_container_token,
          next_container_name,
          atoms,
          rules,
          expressions,
          [#(variable, name), ..variable_names],
          container_names,
          identity_source,
        ),
        name,
      )
    }
  }
}

pub fn container(context: Context) -> #(Context, Container) {
  let Context(
    atom,
    expression,
    next_variable_token,
    next_variable_name,
    next_container_token,
    next_container_name,
    atoms,
    rules,
    expressions,
    variable_names,
    container_names,
    identity_source,
  ) = context
  let token = take_container_token(identity_source)
  let container = Container(new_identity(), token)
  #(
    Context(
      atom,
      expression,
      next_variable_token,
      next_variable_name,
      next_container_token + 1,
      next_container_name,
      atoms,
      rules,
      expressions,
      variable_names,
      container_names,
      identity_source,
    ),
    container,
  )
}

pub fn container_name(
  context: Context,
  container: Container,
) -> #(Context, String) {
  let Context(
    atom,
    expression,
    next_variable_token,
    next_variable_name,
    next_container_token,
    next_container_name,
    atoms,
    rules,
    expressions,
    variable_names,
    container_names,
    identity_source,
  ) = context
  case find_container(container_names, container) {
    Some(name) -> #(context, name)
    None -> {
      let name = "r" <> int_text(next_container_name)
      #(
        Context(
          atom,
          expression,
          next_variable_token,
          next_variable_name,
          next_container_token,
          next_container_name + 1,
          atoms,
          rules,
          expressions,
          variable_names,
          [#(container, name), ..container_names],
          identity_source,
        ),
        name,
      )
    }
  }
}

pub fn container_key(container: Container) -> String {
  let Container(_, token) = container
  "c" <> int.to_string(token)
}

pub fn atom_name(context: Context, key: String) -> #(Context, String, Bool) {
  let Context(
    next_atom,
    expression,
    next_variable_token,
    next_variable_name,
    next_container_token,
    next_container_name,
    atoms,
    rules,
    expressions,
    variable_names,
    container_names,
    identity_source,
  ) = context
  case find_atom(atoms, key) {
    Ok(name) -> #(context, name, False)
    Error(Nil) -> {
      let name = "a" <> int_text(next_atom)
      #(
        Context(
          next_atom + 1,
          expression,
          next_variable_token,
          next_variable_name,
          next_container_token,
          next_container_name,
          [#(key, name), ..atoms],
          rules,
          expressions,
          variable_names,
          container_names,
          identity_source,
        ),
        name,
        True,
      )
    }
  }
}

pub fn remember_atom_rules(
  context: Context,
  key: String,
  rules: List(Rule),
) -> Context {
  let Context(
    atom,
    expression,
    next_variable_token,
    next_variable_name,
    next_container_token,
    next_container_name,
    atoms,
    atom_rules,
    expressions,
    variable_names,
    container_names,
    identity_source,
  ) = context
  Context(
    atom,
    expression,
    next_variable_token,
    next_variable_name,
    next_container_token,
    next_container_name,
    atoms,
    [#(key, rules), ..atom_rules],
    expressions,
    variable_names,
    container_names,
    identity_source,
  )
}

pub fn cached_atom_rules(
  context: Context,
  key: String,
) -> Result(List(Rule), Nil) {
  let Context(_, _, _, _, _, _, _, rules, _, _, _, _) = context
  find_rules(rules, key)
}

pub fn expression_name(
  context: Context,
  key: String,
) -> #(Context, String, String, Bool) {
  let Context(
    atom,
    next_expression,
    next_variable_token,
    next_variable_name,
    next_container_token,
    next_container_name,
    atoms,
    rules,
    expressions,
    variable_names,
    container_names,
    identity_source,
  ) = context
  case find_expression(expressions, key) {
    Ok(#(class_name, property_name)) -> #(
      context,
      class_name,
      property_name,
      False,
    )
    Error(Nil) -> {
      let number = int_text(next_expression)
      let class_name = "e" <> number
      let property_name = "--e" <> number
      #(
        Context(
          atom,
          next_expression + 1,
          next_variable_token,
          next_variable_name,
          next_container_token,
          next_container_name,
          atoms,
          rules,
          [#(key, #(class_name, property_name)), ..expressions],
          variable_names,
          container_names,
          identity_source,
        ),
        class_name,
        property_name,
        True,
      )
    }
  }
}

fn find_atom(
  entries: List(#(String, String)),
  key: String,
) -> Result(String, Nil) {
  case entries {
    [] -> Error(Nil)
    [#(entry_key, value), ..rest] ->
      case entry_key == key {
        True -> Ok(value)
        False -> find_atom(rest, key)
      }
  }
}

fn find_expression(
  entries: List(#(String, #(String, String))),
  key: String,
) -> Result(#(String, String), Nil) {
  case entries {
    [] -> Error(Nil)
    [#(entry_key, value), ..rest] ->
      case entry_key == key {
        True -> Ok(value)
        False -> find_expression(rest, key)
      }
  }
}

fn find_rules(
  entries: List(#(String, List(Rule))),
  key: String,
) -> Result(List(Rule), Nil) {
  case entries {
    [] -> Error(Nil)
    [#(entry_key, value), ..rest] ->
      case entry_key == key {
        True -> Ok(value)
        False -> find_rules(rest, key)
      }
  }
}

fn find_variable(
  entries: List(#(Variable, String)),
  wanted: Variable,
) -> Option(String) {
  case entries {
    [] -> None
    [#(variable, name), ..rest] ->
      case same_variable(variable, wanted) {
        True -> Some(name)
        False -> find_variable(rest, wanted)
      }
  }
}

fn find_container(
  entries: List(#(Container, String)),
  wanted: Container,
) -> Option(String) {
  case entries {
    [] -> None
    [#(container, name), ..rest] ->
      case same_container(container, wanted) {
        True -> Some(name)
        False -> find_container(rest, wanted)
      }
  }
}

fn same_variable(first: Variable, second: Variable) -> Bool {
  let Variable(first_identity, _) = first
  let Variable(second_identity, _) = second
  same_identity(first_identity, second_identity)
}

fn same_container(first: Container, second: Container) -> Bool {
  let Container(first_identity, _) = first
  let Container(second_identity, _) = second
  same_identity(first_identity, second_identity)
}

fn take_variable_token(source: IdentitySource) -> Int {
  let IdentitySource(context, counter) = source
  let #(variable, container) =
    dataflow.value(dataflow.to_computation(dataflow.mutation(counter)))
  let assert Ok(Nil) =
    dataflow.txn(context, fn() {
      dataflow.set(context, counter, #(variable + 1, container))
    })
  variable
}

fn take_container_token(source: IdentitySource) -> Int {
  let IdentitySource(context, counter) = source
  let #(variable, container) =
    dataflow.value(dataflow.to_computation(dataflow.mutation(counter)))
  let assert Ok(Nil) =
    dataflow.txn(context, fn() {
      dataflow.set(context, counter, #(variable, container + 1))
    })
  container
}

fn int_text(value: Int) -> String {
  value
  |> int.to_base16
  |> string.lowercase
}
