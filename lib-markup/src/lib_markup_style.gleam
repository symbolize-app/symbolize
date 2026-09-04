import gleam/list
import gleam/option.{Some}
import lib_dataflow as dataflow
import lib_markup_dom as dom
import lib_markup_fragment as fragment
import lib_markup_scheduler as markup_scheduler
import lib_styling
import lib_styling_container as styling_container
import lib_styling_context as styling_context
import lib_styling_var as styling_var

pub opaque type Host {
  Host(
    document: dom.Document,
    runtime: dataflow.Context,
    scheduler: markup_scheduler.Scheduler,
    compiler: dataflow.Mutation(styling_context.Context),
    sheet: dom.StyleLayer,
    class_names: dataflow.Mutation(List(String)),
  )
}

pub fn new(document: dom.Document) -> Host {
  let runtime = dataflow.dataflow()
  Host(
    document: document,
    runtime: runtime,
    scheduler: markup_scheduler.new(),
    compiler: dataflow.state(lib_styling.styling()),
    sheet: dom.new_style_layer(document),
    class_names: dataflow.state([]),
  )
}

pub fn variable(host: Host) -> #(Host, styling_var.Var(value)) {
  let Host(document, runtime, scheduler, compiler, sheet, class_names_state) =
    host
  let current =
    dataflow.value(dataflow.to_computation(dataflow.mutation(compiler)))
  let #(current, variable) = styling_var.build(current)
  let assert Ok(Nil) =
    dataflow.txn(runtime, fn() { dataflow.set(runtime, compiler, current) })
  #(
    Host(document, runtime, scheduler, compiler, sheet, class_names_state),
    variable,
  )
}

pub fn container(host: Host) -> #(Host, styling_container.Container) {
  let Host(document, runtime, scheduler, compiler, sheet, class_names_state) =
    host
  let current =
    dataflow.value(dataflow.to_computation(dataflow.mutation(compiler)))
  let #(current, container) = styling_container.build(current)
  let assert Ok(Nil) =
    dataflow.txn(runtime, fn() { dataflow.set(runtime, compiler, current) })
  #(
    Host(document, runtime, scheduler, compiler, sheet, class_names_state),
    container,
  )
}

pub fn apply(
  host: Host,
  node: dom.Node,
  value: lib_styling.AtomOpt,
) -> #(Host, List(String)) {
  let #(host, class_names) = compile_style(host, dataflow.literal(value))
  add_classes(node, class_names)
  #(host, class_names)
}

pub fn mount(
  host: Host,
  parent: dom.Node,
  value: fragment.Fragment,
) -> #(Host, dom.Mounted) {
  let #(host, value) = prepare_fragment(host, value)
  let Host(document, _, scheduler, _, sheet, _) = host
  let markup = fragment.markup_context(document, scheduler, Some(sheet))
  let mounted =
    dom.mount_reactive_styled_with_scheduler_and_markup(
      scheduler,
      dataflow.dataflow(),
      markup,
      parent,
      value,
      style_binder(host),
    )
  #(host, mounted)
}

pub fn mount_reactive(
  context: dataflow.Context,
  host: Host,
  parent: dom.Node,
  value: fragment.Fragment,
) -> #(Host, dom.Mounted) {
  let Host(document, _, scheduler, _, sheet, _) = host
  let markup = fragment.markup_context(document, scheduler, Some(sheet))
  let mounted =
    dom.mount_reactive_styled_with_scheduler_and_markup(
      scheduler,
      context,
      markup,
      parent,
      value,
      style_binder(host),
    )
  #(host, mounted)
}

pub fn mount_before_reactive(
  context: dataflow.Context,
  host: Host,
  parent: dom.Node,
  before: dom.Node,
  value: fragment.Fragment,
) -> #(Host, dom.Mounted) {
  let Host(document, _, scheduler, _, sheet, _) = host
  let markup = fragment.markup_context(document, scheduler, Some(sheet))
  let mounted =
    dom.mount_before_reactive_styled_with_scheduler_and_markup(
      scheduler,
      context,
      markup,
      parent,
      before,
      value,
      style_binder(host),
    )
  #(host, mounted)
}

// Source markup contexts expose wait() for the serialized event queue. The
// host owns that queue, so styled callers can wait without reaching into the
// DOM mount representation.
pub fn wait(host: Host, callback: fn() -> Nil) -> Nil {
  let Host(_, _, scheduler, _, _, _) = host
  markup_scheduler.wait(scheduler, callback)
}

fn prepare_fragment(
  host: Host,
  value: fragment.Fragment,
) -> #(Host, fragment.Fragment) {
  case value {
    fragment.Empty -> #(host, value)
    fragment.Text(_) | fragment.ReactiveTextNode(_) -> #(host, value)
    fragment.Range(values) -> {
      let #(host, values) = prepare_fragments(host, values, [])
      #(host, fragment.Range(values))
    }
    fragment.ElementNode(namespace, tag, attributes, content) -> {
      let #(host, attributes) = prepare_attributes(host, attributes, [])
      let #(host, content) = prepare_fragment(host, content)
      #(host, fragment.ElementNode(namespace, tag, attributes, content))
    }
    fragment.PortalNode(node, attributes, content) -> {
      let #(host, attributes) = prepare_attributes(host, attributes, [])
      let #(host, content) = prepare_fragment(host, content)
      #(host, fragment.PortalNode(node, attributes, content))
    }
    fragment.DynamicNode(_) -> #(host, value)
  }
}

fn prepare_fragments(
  host: Host,
  values: List(fragment.Fragment),
  output: List(fragment.Fragment),
) -> #(Host, List(fragment.Fragment)) {
  case values {
    [] -> #(host, reverse(output))
    [first, ..rest] -> {
      let #(host, value) = prepare_fragment(host, first)
      prepare_fragments(host, rest, [value, ..output])
    }
  }
}

fn prepare_attributes(
  host: Host,
  attributes: List(fragment.Attribute),
  output: List(fragment.Attribute),
) -> #(Host, List(fragment.Attribute)) {
  case attributes {
    [] -> #(host, reverse(output))
    [first, ..rest] -> {
      case first {
        fragment.StyleAttribute(value) -> {
          let #(host, class_names) = compile_style(host, value)
          let output = merge_style_classes(output, class_names)
          prepare_attributes(host, rest, output)
        }
        _ -> prepare_attributes(host, rest, [first, ..output])
      }
    }
  }
}

fn compile_style(
  host: Host,
  value: dataflow.NodeOpt(lib_styling.AtomOpt),
) -> #(Host, List(String)) {
  let value = dataflow.value(dataflow.to_computation(value))
  let Host(document, runtime, scheduler, compiler, sheet, class_names_state) =
    host
  let context =
    dataflow.value(dataflow.to_computation(dataflow.mutation(compiler)))
  let #(context, rules, class_names) = lib_styling.compile(context, value)
  let known_class_names =
    dataflow.value(
      dataflow.to_computation(dataflow.mutation(class_names_state)),
    )
  let known_class_names = insert_rules(sheet, known_class_names, rules)
  let assert Ok(Nil) =
    dataflow.txn(runtime, fn() {
      let assert Ok(Nil) = dataflow.set(runtime, compiler, context)
      dataflow.set(runtime, class_names_state, known_class_names)
    })
  #(
    Host(document, runtime, scheduler, compiler, sheet, class_names_state),
    class_names,
  )
}

fn style_binder(host: Host) -> dom.StyleBinder {
  fn(node, value) {
    let old_class_names_state = dataflow.state([])
    dataflow.effect(
      fn(next_value) {
        let old_class_names =
          dataflow.value(
            dataflow.to_computation(dataflow.mutation(old_class_names_state)),
          )
        remove_classes(node, old_class_names)
        let #(host, class_names) =
          compile_style(host, dataflow.literal(next_value))
        add_classes(node, class_names)
        let Host(_, runtime, _, _, _, _) = host
        let assert Ok(Nil) =
          dataflow.txn(runtime, fn() {
            dataflow.set(runtime, old_class_names_state, class_names)
          })
        Nil
      },
      dataflow.to_computation(value),
    )
  }
}

fn merge_style_classes(
  attributes: List(fragment.Attribute),
  class_names: List(String),
) -> List(fragment.Attribute) {
  case class_names {
    [] -> attributes
    _ -> [fragment.StyleClassAttribute(class_names), ..attributes]
  }
}

fn reverse(values: List(a)) -> List(a) {
  reverse_inner(values, [])
}

fn reverse_inner(values: List(a), output: List(a)) -> List(a) {
  case values {
    [] -> output
    [first, ..rest] -> reverse_inner(rest, [first, ..output])
  }
}

fn insert_rules(
  sheet: dom.StyleLayer,
  known_class_names: List(String),
  rules: List(lib_styling.Rule),
) -> List(String) {
  case rules {
    [] -> known_class_names
    [first, ..rest] -> {
      let class_name = styling_context.rule_class_name(first)
      let known_class_names = case
        list.contains(known_class_names, class_name)
      {
        True -> known_class_names
        False -> {
          dom.insert_style_rule(sheet, styling_context.rule_code(first))
          list.append(known_class_names, [class_name])
        }
      }
      insert_rules(sheet, known_class_names, rest)
    }
  }
}

fn add_classes(node: dom.Node, class_names: List(String)) -> Nil {
  case class_names {
    [] -> Nil
    [first, ..rest] -> {
      dom.add_class(node, first)
      add_classes(node, rest)
    }
  }
}

fn remove_classes(node: dom.Node, class_names: List(String)) -> Nil {
  case class_names {
    [] -> Nil
    [first, ..rest] -> {
      dom.remove_class(node, first)
      remove_classes(node, rest)
    }
  }
}
