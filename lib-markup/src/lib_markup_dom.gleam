import gleam/option.{type Option, None, Some}
import lib_dataflow as dataflow
import lib_markup_fragment as fragment
import lib_markup_scheduler as markup_scheduler
import lib_styling_atom as atom

pub type Document =
  fragment.Document

pub type Node =
  fragment.Node

pub type Event =
  fragment.Event

pub type EventSubscription =
  fragment.EventSubscription

pub type StyleLayer =
  fragment.StyleLayer

pub type StyleBinder =
  fragment.StyleBinder

pub opaque type Mounted {
  Mounted(
    parent: Node,
    nodes: List(Node),
    subscriptions: List(dataflow.Subscription),
    event_subscriptions: List(EventSubscription),
    cleanups: List(fn() -> Nil),
    scheduler: markup_scheduler.Scheduler,
  )
}

type Restore {
  Restore(name: String, value: Option(String))
}

@external(javascript, "./dom_ffi.mjs", "document")
pub fn document() -> Document

@external(javascript, "./dom_ffi.mjs", "body")
pub fn body(document: Document) -> Node

@external(javascript, "./dom_ffi.mjs", "document_element")
pub fn document_element(document: Document) -> Node

@external(javascript, "./dom_ffi.mjs", "head")
pub fn head(document: Document) -> Node

@external(javascript, "./dom_ffi.mjs", "remove_first_title")
pub fn remove_first_title(node: Node) -> Nil

@external(javascript, "./dom_ffi.mjs", "create_element")
fn create_html_element(document: Document, tag: String) -> Node

pub fn create_html_node(document: Document, tag: String) -> Node {
  create_html_element(document, tag)
}

@external(javascript, "./dom_ffi.mjs", "create_element_ns")
fn create_namespaced_element(
  document: Document,
  namespace_uri: String,
  tag: String,
) -> Node

@external(javascript, "./dom_ffi.mjs", "create_text")
fn create_text(document: Document, value: String) -> Node

pub fn create_text_node(document: Document, value: String) -> Node {
  create_text(document, value)
}

@external(javascript, "./dom_ffi.mjs", "set_attribute")
fn set_attribute(node: Node, name: String, value: String) -> Nil

pub fn set_attribute_value(node: Node, name: String, value: String) -> Nil {
  set_attribute(node, name, value)
}

@external(javascript, "./dom_ffi.mjs", "remove_attribute")
fn remove_attribute(node: Node, name: String) -> Nil

@external(javascript, "./dom_ffi.mjs", "has_attribute")
fn has_attribute(node: Node, name: String) -> Bool

@external(javascript, "./dom_ffi.mjs", "boolean_property")
pub fn boolean_property(node: Node, name: String) -> Bool

pub fn has_attribute_value(node: Node, name: String) -> Bool {
  has_attribute(node, name)
}

@external(javascript, "./dom_ffi.mjs", "get_attribute_value")
fn get_attribute_value(node: Node, name: String) -> String

pub fn remove_attribute_value(node: Node, name: String) -> Nil {
  remove_attribute(node, name)
}

@external(javascript, "./dom_ffi.mjs", "append_child")
fn append_child(parent: Node, child: Node) -> Nil

pub fn append_node(parent: Node, child: Node) -> Nil {
  append_child(parent, child)
}

pub fn move_before(parent: Node, nodes: List(Node), before: Node) -> Nil {
  case nodes {
    [] -> Nil
    [first, ..rest] -> {
      insert_before(parent, first, before)
      move_before(parent, rest, before)
    }
  }
}

@external(javascript, "./dom_ffi.mjs", "remove_child")
fn remove_child(parent: Node, child: Node) -> Nil

pub fn remove_node(parent: Node, child: Node) -> Nil {
  remove_child(parent, child)
}

@external(javascript, "./dom_ffi.mjs", "set_text_content")
fn set_text_content_ffi(node: Node, value: String) -> Nil

pub fn set_text_content(node: Node, value: String) -> Nil {
  set_text_content_ffi(node, value)
}

@external(javascript, "./dom_ffi.mjs", "create_comment")
pub fn create_comment(document: Document, value: String) -> Node

@external(javascript, "./dom_ffi.mjs", "outer_html")
pub fn outer_html(node: Node) -> String

@external(javascript, "./dom_ffi.mjs", "is_connected")
pub fn is_connected(node: Node) -> Bool

@external(javascript, "./dom_ffi.mjs", "add_event_listener")
pub fn add_event_listener(
  node: Node,
  name: String,
  callback: fn(Event) -> Nil,
) -> EventSubscription

@external(javascript, "./dom_ffi.mjs", "remove_event_listener")
pub fn remove_event_listener(subscription: EventSubscription) -> Nil

@external(javascript, "./dom_ffi.mjs", "click")
pub fn click(node: Node) -> Nil

@external(javascript, "./dom_ffi.mjs", "dispatch_event")
pub fn dispatch_event(node: Node, name: String) -> Nil

@external(javascript, "./dom_ffi.mjs", "dispatch_keyboard_event")
pub fn dispatch_keyboard_event(node: Node, name: String, key: String) -> Nil

@external(javascript, "./dom_ffi.mjs", "new_style_layer")
pub fn new_style_layer(document: Document) -> StyleLayer

@external(javascript, "./dom_ffi.mjs", "insert_style_rule")
pub fn insert_style_rule(layer: StyleLayer, code: String) -> Nil

@external(javascript, "./dom_ffi.mjs", "add_class")
pub fn add_class(node: Node, name: String) -> Nil

@external(javascript, "./dom_ffi.mjs", "remove_class")
pub fn remove_class(node: Node, name: String) -> Nil

@external(javascript, "./dom_ffi.mjs", "class_name")
pub fn class_name(node: Node) -> String

@external(javascript, "./dom_ffi.mjs", "computed_style")
pub fn computed_style(node: Node, property: String) -> String

@external(javascript, "./dom_ffi.mjs", "insert_before")
fn insert_before(parent: Node, child: Node, before: Node) -> Nil

@external(javascript, "./dom_ffi.mjs", "text_content")
pub fn text_content(node: Node) -> String

pub fn mount(
  document: Document,
  parent: Node,
  value: fragment.Fragment,
) -> Mounted {
  mount_reactive(dataflow.dataflow(), document, parent, value)
}

pub fn mount_reactive(
  context: dataflow.Context,
  document: Document,
  parent: Node,
  value: fragment.Fragment,
) -> Mounted {
  mount_reactive_with_scheduler(
    markup_scheduler.new(),
    context,
    document,
    parent,
    value,
  )
}

pub fn mount_reactive_with_scheduler(
  scheduler: markup_scheduler.Scheduler,
  context: dataflow.Context,
  document: Document,
  parent: Node,
  value: fragment.Fragment,
) -> Mounted {
  let markup = fragment.markup_context(document, scheduler, None)
  let #(nodes, subscriptions, event_subscriptions, cleanups, _pending) =
    mount_fragment_reactive(
      document,
      parent,
      value,
      context,
      scheduler,
      markup,
      None,
      None,
      False,
    )
  Mounted(
    parent: parent,
    nodes: nodes,
    subscriptions: subscriptions,
    event_subscriptions: event_subscriptions,
    cleanups: cleanups,
    scheduler: scheduler,
  )
}

pub fn mount_reactive_styled_with_scheduler_and_markup(
  scheduler: markup_scheduler.Scheduler,
  context: dataflow.Context,
  markup: fragment.MarkupContext,
  parent: Node,
  value: fragment.Fragment,
  style_binder: StyleBinder,
) -> Mounted {
  let document = fragment.markup_document(markup)
  let #(nodes, subscriptions, event_subscriptions, cleanups, _pending) =
    mount_fragment_reactive(
      document,
      parent,
      value,
      context,
      scheduler,
      markup,
      None,
      Some(style_binder),
      False,
    )
  Mounted(
    parent: parent,
    nodes: nodes,
    subscriptions: subscriptions,
    event_subscriptions: event_subscriptions,
    cleanups: cleanups,
    scheduler: scheduler,
  )
}

pub fn mount_reactive_styled(
  context: dataflow.Context,
  document: Document,
  parent: Node,
  value: fragment.Fragment,
  style_binder: StyleBinder,
) -> Mounted {
  mount_reactive_styled_with_scheduler(
    markup_scheduler.new(),
    context,
    document,
    parent,
    value,
    style_binder,
  )
}

pub fn mount_reactive_styled_with_scheduler(
  scheduler: markup_scheduler.Scheduler,
  context: dataflow.Context,
  document: Document,
  parent: Node,
  value: fragment.Fragment,
  style_binder: StyleBinder,
) -> Mounted {
  let markup = fragment.markup_context(document, scheduler, None)
  mount_reactive_styled_with_scheduler_and_markup(
    scheduler,
    context,
    markup,
    parent,
    value,
    style_binder,
  )
}

pub fn mount_before(
  document: Document,
  parent: Node,
  before: Node,
  value: fragment.Fragment,
) -> Mounted {
  mount_before_with_scheduler(
    markup_scheduler.new(),
    document,
    parent,
    before,
    value,
  )
}

fn mount_before_with_scheduler(
  scheduler: markup_scheduler.Scheduler,
  document: Document,
  parent: Node,
  before: Node,
  value: fragment.Fragment,
) -> Mounted {
  let #(nodes, event_subscriptions, cleanups) =
    mount_fragment_at(document, parent, value, scheduler, Some(before))
  Mounted(
    parent: parent,
    nodes: nodes,
    subscriptions: [],
    event_subscriptions: event_subscriptions,
    cleanups: cleanups,
    scheduler: scheduler,
  )
}

pub fn mount_before_reactive(
  context: dataflow.Context,
  document: Document,
  parent: Node,
  before: Node,
  value: fragment.Fragment,
) -> Mounted {
  mount_before_reactive_with_scheduler(
    markup_scheduler.new(),
    context,
    document,
    parent,
    before,
    value,
  )
}

pub fn mount_before_reactive_with_scheduler(
  scheduler: markup_scheduler.Scheduler,
  context: dataflow.Context,
  document: Document,
  parent: Node,
  before: Node,
  value: fragment.Fragment,
) -> Mounted {
  let markup = fragment.markup_context(document, scheduler, None)
  let #(nodes, subscriptions, event_subscriptions, cleanups, _pending) =
    mount_fragment_reactive(
      document,
      parent,
      value,
      context,
      scheduler,
      markup,
      Some(before),
      None,
      False,
    )
  Mounted(
    parent: parent,
    nodes: nodes,
    subscriptions: subscriptions,
    event_subscriptions: event_subscriptions,
    cleanups: cleanups,
    scheduler: scheduler,
  )
}

pub fn mount_before_reactive_styled(
  context: dataflow.Context,
  document: Document,
  parent: Node,
  before: Node,
  value: fragment.Fragment,
  style_binder: StyleBinder,
) -> Mounted {
  mount_before_reactive_styled_with_scheduler(
    markup_scheduler.new(),
    context,
    document,
    parent,
    before,
    value,
    style_binder,
  )
}

pub fn mount_before_reactive_styled_with_scheduler(
  scheduler: markup_scheduler.Scheduler,
  context: dataflow.Context,
  document: Document,
  parent: Node,
  before: Node,
  value: fragment.Fragment,
  style_binder: StyleBinder,
) -> Mounted {
  let markup = fragment.markup_context(document, scheduler, None)
  mount_before_reactive_styled_with_scheduler_and_markup(
    scheduler,
    context,
    markup,
    parent,
    before,
    value,
    style_binder,
  )
}

pub fn mount_before_reactive_styled_with_scheduler_and_markup(
  scheduler: markup_scheduler.Scheduler,
  context: dataflow.Context,
  markup: fragment.MarkupContext,
  parent: Node,
  before: Node,
  value: fragment.Fragment,
  style_binder: StyleBinder,
) -> Mounted {
  let document = fragment.markup_document(markup)
  let #(nodes, subscriptions, event_subscriptions, cleanups, _pending) =
    mount_fragment_reactive(
      document,
      parent,
      value,
      context,
      scheduler,
      markup,
      Some(before),
      Some(style_binder),
      False,
    )
  Mounted(
    parent: parent,
    nodes: nodes,
    subscriptions: subscriptions,
    event_subscriptions: event_subscriptions,
    cleanups: cleanups,
    scheduler: scheduler,
  )
}

pub fn nodes(mounted: Mounted) -> List(Node) {
  let Mounted(nodes: nodes, ..) = mounted
  nodes
}

pub fn first(mounted: Mounted) -> Node {
  let Mounted(nodes: nodes, ..) = mounted
  case nodes {
    [node, ..] -> node
    [] -> panic as "mounted fragment has no nodes"
  }
}

pub fn wait(mounted: Mounted, callback: fn() -> Nil) -> Nil {
  let Mounted(scheduler: scheduler, ..) = mounted
  markup_scheduler.wait(scheduler, callback)
}

pub fn wait_result(
  mounted: Mounted,
  callback: fn(Result(Nil, String)) -> Nil,
) -> Nil {
  let Mounted(scheduler: scheduler, ..) = mounted
  markup_scheduler.wait_result(scheduler, callback)
}

pub fn remove(mounted: Mounted) -> Nil {
  let Mounted(
    parent: parent,
    nodes: nodes,
    subscriptions: subscriptions,
    event_subscriptions: event_subscriptions,
    cleanups: cleanups,
    scheduler: _,
  ) = mounted
  run_cleanups(cleanups)
  unsubscribe_all(subscriptions)
  unsubscribe_events(event_subscriptions)
  remove_nodes(parent, nodes)
}

fn mount_fragment_at(
  document: Document,
  parent: Node,
  value: fragment.Fragment,
  scheduler: markup_scheduler.Scheduler,
  before: Option(Node),
) -> #(List(Node), List(EventSubscription), List(fn() -> Nil)) {
  case value {
    fragment.Empty -> #([], [], [])
    fragment.Text(value) -> {
      let node = create_text(document, value)
      insert_or_append(parent, node, before)
      #([node], [], [])
    }
    fragment.ReactiveTextNode(value) -> {
      let node =
        create_text(document, dataflow.value(dataflow.to_computation(value)))
      insert_or_append(parent, node, before)
      #([node], [], [])
    }
    fragment.Range(values) ->
      mount_many_at(document, parent, values, scheduler, before)
    fragment.ElementNode(namespace, tag, attributes, content) -> {
      let node = create_element(document, namespace, tag)
      let event_subscriptions = set_attributes(node, attributes, scheduler)
      insert_or_append(parent, node, before)
      let #(_content_nodes, content_event_subscriptions, content_cleanups) =
        mount_fragment_at(document, node, content, scheduler, None)
      #(
        [node],
        append(event_subscriptions, content_event_subscriptions),
        content_cleanups,
      )
    }
    fragment.PortalNode(node, attributes, content) -> {
      let #(restores, event_subscriptions) =
        set_portal_attributes(node, attributes, scheduler)
      let #(content_nodes, content_event_subscriptions, content_cleanups) =
        mount_fragment_at(document, node, content, scheduler, None)
      let cleanup = fn() {
        remove_nodes(node, content_nodes)
        run_cleanups(content_cleanups)
        restore_attributes(node, restores)
      }
      #([], append(event_subscriptions, content_event_subscriptions), [cleanup])
    }
    fragment.DynamicNode(_) ->
      panic as "dynamic fragments require a dynamic mount context"
  }
}

fn create_element(
  document: Document,
  namespace: fragment.Namespace,
  tag: String,
) -> Node {
  case namespace {
    fragment.Html -> create_html_element(document, tag)
    fragment.Svg ->
      create_namespaced_element(document, "http://www.w3.org/2000/svg", tag)
    fragment.MathMl ->
      create_namespaced_element(
        document,
        "http://www.w3.org/1998/Math/MathML",
        tag,
      )
  }
}

fn mount_many_at(
  document: Document,
  parent: Node,
  values: List(fragment.Fragment),
  scheduler: markup_scheduler.Scheduler,
  before: Option(Node),
) -> #(List(Node), List(EventSubscription), List(fn() -> Nil)) {
  case values {
    [] -> #([], [], [])
    [first, ..rest] -> {
      let #(first_nodes, first_events, first_cleanups) =
        mount_fragment_at(document, parent, first, scheduler, before)
      let #(rest_nodes, rest_events, rest_cleanups) =
        mount_many_at(document, parent, rest, scheduler, before)
      #(
        append(first_nodes, rest_nodes),
        append(first_events, rest_events),
        append(first_cleanups, rest_cleanups),
      )
    }
  }
}

fn insert_or_append(parent: Node, child: Node, before: Option(Node)) -> Nil {
  case before {
    None -> append_child(parent, child)
    Some(before) -> insert_before(parent, child, before)
  }
}

fn set_attributes(
  node: Node,
  attributes: List(fragment.Attribute),
  scheduler: markup_scheduler.Scheduler,
) -> List(EventSubscription) {
  case attributes {
    [] -> []
    [first, ..rest] -> {
      let event_subscriptions = case first {
        fragment.StringAttribute(name, value) -> {
          set_attribute(node, name, value)
          []
        }
        fragment.BooleanAttribute(name, True) -> {
          set_attribute(node, name, "")
          []
        }
        fragment.BooleanAttribute(_, False) -> []
        fragment.ClassAttribute(values) -> {
          set_attribute(node, "class", join(" ", values))
          []
        }
        fragment.StyleClassAttribute(values) -> {
          add_classes(node, values)
          []
        }
        fragment.OnAddAttribute(_) -> []
        fragment.EventAttribute(name, listener) -> [
          add_event_listener(
            node,
            name,
            scheduled_listener(scheduler, listener),
          ),
        ]
        fragment.StyleAttribute(value) -> {
          case dataflow.value(dataflow.to_computation(value)) {
            atom.Empty -> []
            _ -> panic as "style attributes require lib_markup_style.mount"
          }
        }
        fragment.ReactiveOptionalStringAttribute(name, value) -> {
          set_optional_string_attribute(
            node,
            name,
            dataflow.value(dataflow.to_computation(value)),
          )
          []
        }
        fragment.ReactiveOptionalBooleanStringAttribute(name, value) -> {
          set_optional_bool_string_attribute(
            node,
            name,
            dataflow.value(dataflow.to_computation(value)),
          )
          []
        }
        fragment.ReactiveOptionalBooleanAttribute(name, value) -> {
          set_optional_boolean_attribute(
            node,
            name,
            dataflow.value(dataflow.to_computation(value)),
          )
          []
        }
        fragment.ReactiveOptionalClassAttribute(value) -> {
          set_optional_class_attribute(
            node,
            dataflow.value(dataflow.to_computation(value)),
          )
          []
        }
      }
      append(event_subscriptions, set_attributes(node, rest, scheduler))
    }
  }
}

fn mount_fragment_reactive(
  document: Document,
  parent: Node,
  value: fragment.Fragment,
  context: dataflow.Context,
  scheduler: markup_scheduler.Scheduler,
  markup: fragment.MarkupContext,
  before: Option(Node),
  style_binder: Option(StyleBinder),
  deferred: Bool,
) -> #(
  List(Node),
  List(dataflow.Subscription),
  List(EventSubscription),
  List(fn() -> Nil),
  Bool,
) {
  case value {
    fragment.Empty -> #([], [], [], [], False)
    fragment.Text(value) -> {
      let node = create_text(document, value)
      case deferred {
        True ->
          markup_scheduler.run(scheduler, fn() {
            insert_or_append(parent, node, before)
          })
        False -> insert_or_append(parent, node, before)
      }
      #([node], [], [], [], deferred)
    }
    fragment.ReactiveTextNode(value) -> {
      let node =
        create_text(document, dataflow.value(dataflow.to_computation(value)))
      case deferred {
        True ->
          markup_scheduler.run(scheduler, fn() {
            insert_or_append(parent, node, before)
          })
        False -> insert_or_append(parent, node, before)
      }
      let subscription =
        dataflow.effect(
          fn(next_value) { set_text_content(node, next_value) },
          dataflow.to_computation(value),
        )
      #([node], [subscription], [], [], deferred)
    }
    fragment.Range(values) ->
      mount_many_reactive(
        document,
        parent,
        values,
        context,
        scheduler,
        markup,
        before,
        style_binder,
        deferred,
      )
    fragment.ElementNode(namespace, tag, attributes, content) -> {
      let node = create_element(document, namespace, tag)
      let #(subscriptions, event_subscriptions, on_adds) =
        set_attributes_reactive(
          node,
          attributes,
          context,
          scheduler,
          style_binder,
        )
      let #(
        _content_nodes,
        content_subscriptions,
        content_events,
        content_cleanups,
        content_pending,
      ) =
        mount_fragment_reactive(
          document,
          node,
          content,
          context,
          scheduler,
          markup,
          None,
          style_binder,
          deferred,
        )
      let removed = dataflow.state(False)
      let #(scoped_subscriptions, scoped_cleanups, on_add_pending) =
        run_on_adds(
          node,
          on_adds,
          context,
          markup,
          scheduler,
          deferred || content_pending,
          removed,
        )
      // A normal source element is appended by its containing fragment only
      // after `add` has finished, including an async `onAdd`. Keep the node
      // detached until the scheduler has completed that lifecycle callback.
      case on_add_pending {
        True ->
          markup_scheduler.run(scheduler, fn() {
            case
              dataflow.value(
                dataflow.to_computation(dataflow.mutation(removed)),
              )
            {
              True -> Nil
              False -> insert_or_append(parent, node, before)
            }
          })
        False -> insert_or_append(parent, node, before)
      }
      #(
        [node],
        append(
          append(subscriptions, content_subscriptions),
          scoped_subscriptions,
        ),
        append(event_subscriptions, content_events),
        [
          fn() {
            let assert Ok(Nil) =
              dataflow.txn(context, fn() {
                dataflow.set(context, removed, True)
              })
            Nil
          },
          ..append(scoped_cleanups, content_cleanups)
        ],
        on_add_pending,
      )
    }
    fragment.PortalNode(node, attributes, content) -> {
      let #(restores, attribute_subscriptions, event_subscriptions, on_adds) =
        set_portal_attributes_reactive(
          node,
          attributes,
          context,
          scheduler,
          style_binder,
        )
      let #(
        content_nodes,
        content_subscriptions,
        content_event_subscriptions,
        content_cleanups,
        content_pending,
      ) =
        mount_fragment_reactive(
          document,
          node,
          content,
          context,
          scheduler,
          markup,
          None,
          style_binder,
          deferred,
        )
      let removed = dataflow.state(False)
      let #(scoped_subscriptions, scoped_cleanups, on_add_pending) =
        run_on_adds(
          node,
          on_adds,
          context,
          markup,
          scheduler,
          deferred || content_pending,
          removed,
        )
      let cleanup = fn() {
        let assert Ok(Nil) =
          dataflow.txn(context, fn() { dataflow.set(context, removed, True) })
        run_cleanups(scoped_cleanups)
        run_cleanups(content_cleanups)
        unsubscribe_events(content_event_subscriptions)
        remove_nodes(node, content_nodes)
        restore_attributes(node, restores)
      }
      #(
        [],
        append(
          append(attribute_subscriptions, content_subscriptions),
          scoped_subscriptions,
        ),
        append(event_subscriptions, content_event_subscriptions),
        [cleanup],
        on_add_pending,
      )
    }
    fragment.DynamicNode(fragment.Dynamic(mount)) -> {
      let mounted =
        mount(mount_context(
          document,
          parent,
          before,
          context,
          markup,
          scheduler,
          style_binder,
          deferred,
        ))
      let fragment.MountedContent(
        nodes: nodes,
        subscriptions: subscriptions,
        event_subscriptions: event_subscriptions,
        cleanups: cleanups,
        pending: pending,
        ..,
      ) = mounted
      #(nodes, subscriptions, event_subscriptions, cleanups, pending)
    }
  }
}

fn mount_many_reactive(
  document: Document,
  parent: Node,
  values: List(fragment.Fragment),
  context: dataflow.Context,
  scheduler: markup_scheduler.Scheduler,
  markup: fragment.MarkupContext,
  before: Option(Node),
  style_binder: Option(StyleBinder),
  deferred: Bool,
) -> #(
  List(Node),
  List(dataflow.Subscription),
  List(EventSubscription),
  List(fn() -> Nil),
  Bool,
) {
  case values {
    [] -> #([], [], [], [], False)
    [first, ..rest] -> {
      let #(
        first_nodes,
        first_subscriptions,
        first_events,
        first_cleanups,
        first_pending,
      ) =
        mount_fragment_reactive(
          document,
          parent,
          first,
          context,
          scheduler,
          markup,
          before,
          style_binder,
          deferred,
        )
      let #(
        rest_nodes,
        rest_subscriptions,
        rest_events,
        rest_cleanups,
        rest_pending,
      ) =
        mount_many_reactive(
          document,
          parent,
          rest,
          context,
          scheduler,
          markup,
          before,
          style_binder,
          deferred || first_pending,
        )
      #(
        append(first_nodes, rest_nodes),
        append(first_subscriptions, rest_subscriptions),
        append(first_events, rest_events),
        append(first_cleanups, rest_cleanups),
        first_pending || rest_pending,
      )
    }
  }
}

fn mount_context(
  document: Document,
  parent: Node,
  before: Option(Node),
  runtime: dataflow.Context,
  markup: fragment.MarkupContext,
  scheduler: markup_scheduler.Scheduler,
  style_binder: Option(StyleBinder),
  deferred: Bool,
) -> fragment.MountContext {
  fragment.MountContext(
    document: document,
    parent: parent,
    before: before,
    runtime: runtime,
    markup: markup,
    scheduler: scheduler,
    style_binder: style_binder,
    deferred: deferred,
    mount: mount_content,
    remove: remove_content,
    create_comment: create_comment,
    append_node: append_child,
    insert_before: insert_before,
    remove_node: remove_child,
  )
}

fn mount_content(
  context: fragment.MountContext,
  value: fragment.Fragment,
) -> fragment.MountedContent {
  let fragment.MountContext(
    document: document,
    parent: parent,
    before: before,
    runtime: runtime,
    markup: markup,
    scheduler: scheduler,
    style_binder: style_binder,
    deferred: deferred,
    ..,
  ) = context
  let #(nodes, subscriptions, event_subscriptions, cleanups, pending) =
    mount_fragment_reactive(
      document,
      parent,
      value,
      runtime,
      scheduler,
      markup,
      before,
      style_binder,
      deferred,
    )
  fragment.MountedContent(
    parent: parent,
    nodes: nodes,
    subscriptions: subscriptions,
    event_subscriptions: event_subscriptions,
    cleanups: cleanups,
    scheduler: scheduler,
    pending: pending,
  )
}

fn remove_content(mounted: fragment.MountedContent) -> Nil {
  let fragment.MountedContent(
    parent: parent,
    nodes: nodes,
    subscriptions: subscriptions,
    event_subscriptions: event_subscriptions,
    cleanups: cleanups,
    scheduler: _,
    pending: _,
  ) = mounted
  run_cleanups(cleanups)
  unsubscribe_all(subscriptions)
  unsubscribe_events(event_subscriptions)
  remove_nodes(parent, nodes)
}

fn set_attributes_reactive(
  node: Node,
  attributes: List(fragment.Attribute),
  context: dataflow.Context,
  scheduler: markup_scheduler.Scheduler,
  style_binder: Option(StyleBinder),
) -> #(
  List(dataflow.Subscription),
  List(EventSubscription),
  List(fragment.OnAdd),
) {
  let _ = context
  case attributes {
    [] -> #([], [], [])
    [first, ..rest] -> {
      let #(subscriptions, event_subscriptions, on_adds) = case first {
        fragment.StringAttribute(name, value) -> {
          set_attribute(node, name, value)
          #([], [], [])
        }
        fragment.BooleanAttribute(name, True) -> {
          set_attribute(node, name, "")
          #([], [], [])
        }
        fragment.BooleanAttribute(_, False) -> #([], [], [])
        fragment.ClassAttribute(values) -> {
          set_attribute(node, "class", join(" ", values))
          #([], [], [])
        }
        fragment.StyleClassAttribute(values) -> {
          add_classes(node, values)
          #([], [], [])
        }
        fragment.OnAddAttribute(callback) -> {
          #([], [], [callback])
        }
        fragment.EventAttribute(name, listener) -> {
          #(
            [],
            [
              add_event_listener(
                node,
                name,
                scheduled_transaction_listener(scheduler, context, listener),
              ),
            ],
            [],
          )
        }
        fragment.StyleAttribute(value) -> {
          case style_binder {
            Some(bind_style) -> #([bind_style(node, value)], [], [])
            None ->
              case dataflow.value(dataflow.to_computation(value)) {
                atom.Empty -> #([], [], [])
                _ -> panic as "style attributes require lib_markup_style.mount"
              }
          }
        }
        fragment.ReactiveOptionalStringAttribute(name, value) -> #(
          [
            dataflow.effect(
              fn(next_value) {
                set_optional_string_attribute(node, name, next_value)
              },
              dataflow.to_computation(value),
            ),
          ],
          [],
          [],
        )
        fragment.ReactiveOptionalBooleanStringAttribute(name, value) -> #(
          [
            dataflow.effect(
              fn(next_value) {
                set_optional_bool_string_attribute(node, name, next_value)
              },
              dataflow.to_computation(value),
            ),
          ],
          [],
          [],
        )
        fragment.ReactiveOptionalBooleanAttribute(name, value) -> #(
          [
            dataflow.effect(
              fn(next_value) {
                set_optional_boolean_attribute(node, name, next_value)
              },
              dataflow.to_computation(value),
            ),
          ],
          [],
          [],
        )
        fragment.ReactiveOptionalClassAttribute(value) -> #(
          [
            dataflow.effect(
              fn(next_value) { set_optional_class_attribute(node, next_value) },
              dataflow.to_computation(value),
            ),
          ],
          [],
          [],
        )
      }
      let #(rest_subscriptions, rest_events, rest_on_adds) =
        set_attributes_reactive(node, rest, context, scheduler, style_binder)
      #(
        append(subscriptions, rest_subscriptions),
        append(event_subscriptions, rest_events),
        append(on_adds, rest_on_adds),
      )
    }
  }
}

fn run_on_adds(
  node: Node,
  callbacks: List(fragment.OnAdd),
  context: dataflow.Context,
  markup: fragment.MarkupContext,
  scheduler: markup_scheduler.Scheduler,
  pending: Bool,
  removed: dataflow.Mutation(Bool),
) -> #(List(dataflow.Subscription), List(fn() -> Nil), Bool) {
  case callbacks {
    [] -> #([], [], pending)
    _ -> {
      let resource_context = dataflow.dataflow()
      let subscriptions = dataflow.state([])
      let cleanups = dataflow.state([])
      let scoped =
        fragment.ScopedContext(
          context: context,
          markup: markup,
          node: node,
          scheduler: scheduler,
          subscribe: fn(subscription) {
            let assert Ok(Nil) =
              dataflow.txn(resource_context, fn() {
                let current =
                  dataflow.value(
                    dataflow.to_computation(dataflow.mutation(subscriptions)),
                  )
                dataflow.set(resource_context, subscriptions, [
                  subscription,
                  ..current
                ])
              })
            Nil
          },
          defer: fn(cleanup) {
            let assert Ok(Nil) =
              dataflow.txn(resource_context, fn() {
                let current =
                  dataflow.value(
                    dataflow.to_computation(dataflow.mutation(cleanups)),
                  )
                dataflow.set(resource_context, cleanups, [cleanup, ..current])
              })
            Nil
          },
        )
      let on_add_pending =
        run_on_add_callbacks(
          callbacks,
          fragment.OnAddEvent(scoped, node),
          scheduler,
          pending,
          removed,
        )
      let cleanup = fn() {
        run_cleanups(
          dataflow.value(dataflow.to_computation(dataflow.mutation(cleanups))),
        )
      }
      let subscription =
        dataflow.subscription(fn() {
          unsubscribe_all(
            dataflow.value(
              dataflow.to_computation(dataflow.mutation(subscriptions)),
            ),
          )
        })
      #([subscription], [cleanup], on_add_pending)
    }
  }
}

fn run_on_add_callbacks(
  callbacks: List(fragment.OnAdd),
  event: fragment.OnAddEvent,
  scheduler: markup_scheduler.Scheduler,
  pending: Bool,
  removed: dataflow.Mutation(Bool),
) -> Bool {
  case callbacks {
    [] -> pending
    [first, ..rest] -> {
      let pending = case first {
        fragment.SyncOnAdd(callback) -> {
          let invoke = fn() {
            case
              dataflow.value(
                dataflow.to_computation(dataflow.mutation(removed)),
              )
            {
              True -> Nil
              False -> callback(event)
            }
          }
          case pending {
            True -> markup_scheduler.run(scheduler, invoke)
            False -> invoke()
          }
          pending
        }
        fragment.AsyncOnAdd(callback) -> {
          markup_scheduler.run_async_result(scheduler, fn(done) {
            case
              dataflow.value(
                dataflow.to_computation(dataflow.mutation(removed)),
              )
            {
              True -> done(Ok(Nil))
              False -> callback(event)(done)
            }
          })
          True
        }
      }
      run_on_add_callbacks(rest, event, scheduler, pending, removed)
    }
  }
}

fn set_optional_string_attribute(
  node: Node,
  name: String,
  value: Option(String),
) -> Nil {
  case value {
    None -> remove_attribute(node, name)
    Some(value) -> set_attribute(node, name, value)
  }
}

fn set_optional_bool_string_attribute(
  node: Node,
  name: String,
  value: Option(Bool),
) -> Nil {
  case value {
    None -> remove_attribute(node, name)
    Some(value) -> set_attribute(node, name, bool_text(value))
  }
}

fn set_optional_boolean_attribute(
  node: Node,
  name: String,
  value: Option(Bool),
) -> Nil {
  case value {
    Some(True) -> set_attribute(node, name, "")
    _ -> remove_attribute(node, name)
  }
}

fn set_optional_class_attribute(
  node: Node,
  value: Option(List(String)),
) -> Nil {
  case value {
    None -> remove_attribute(node, "class")
    Some(value) -> set_attribute(node, "class", join(" ", value))
  }
}

fn bool_text(value: Bool) -> String {
  case value {
    True -> "true"
    False -> "false"
  }
}

fn unsubscribe_all(subscriptions: List(dataflow.Subscription)) -> Nil {
  case subscriptions {
    [] -> Nil
    [first, ..rest] -> {
      dataflow.unsubscribe(first)
      unsubscribe_all(rest)
    }
  }
}

fn unsubscribe_events(subscriptions: List(EventSubscription)) -> Nil {
  case subscriptions {
    [] -> Nil
    [first, ..rest] -> {
      remove_event_listener(first)
      unsubscribe_events(rest)
    }
  }
}

fn remove_nodes(parent: Node, nodes: List(Node)) -> Nil {
  case nodes {
    [] -> Nil
    [first, ..rest] -> {
      remove_child(parent, first)
      remove_nodes(parent, rest)
    }
  }
}

fn add_classes(node: Node, class_names: List(String)) -> Nil {
  case class_names {
    [] -> Nil
    [first, ..rest] -> {
      add_class(node, first)
      add_classes(node, rest)
    }
  }
}

fn set_portal_attributes(
  node: Node,
  attributes: List(fragment.Attribute),
  scheduler: markup_scheduler.Scheduler,
) -> #(List(Restore), List(EventSubscription)) {
  case attributes {
    [] -> #([], [])
    [first, ..rest] -> {
      let #(first_restores, first_events) =
        set_portal_attribute(node, first, scheduler)
      let #(rest_restores, rest_events) =
        set_portal_attributes(node, rest, scheduler)
      #(
        append(first_restores, rest_restores),
        append(first_events, rest_events),
      )
    }
  }
}

fn set_portal_attribute(
  node: Node,
  attribute: fragment.Attribute,
  scheduler: markup_scheduler.Scheduler,
) -> #(List(Restore), List(EventSubscription)) {
  case attribute {
    fragment.StringAttribute(name, value) -> {
      let restore = Restore(name, current_attribute(node, name))
      set_attribute(node, name, value)
      #([restore], [])
    }
    fragment.BooleanAttribute(name, True) -> {
      let restore = Restore(name, current_attribute(node, name))
      set_attribute(node, name, "")
      #([restore], [])
    }
    fragment.BooleanAttribute(_, False) -> #([], [])
    fragment.ClassAttribute(values) -> {
      let restore = Restore("class", current_attribute(node, "class"))
      set_attribute(node, "class", join(" ", values))
      #([restore], [])
    }
    fragment.StyleClassAttribute(values) -> {
      let restore = Restore("class", current_attribute(node, "class"))
      add_classes(node, values)
      #([restore], [])
    }
    fragment.OnAddAttribute(_) -> #([], [])
    fragment.EventAttribute(name, listener) -> #([], [
      add_event_listener(node, name, scheduled_listener(scheduler, listener)),
    ])
    fragment.StyleAttribute(value) -> {
      case dataflow.value(dataflow.to_computation(value)) {
        atom.Empty -> #([], [])
        _ -> panic as "style attributes require lib_markup_style.mount"
      }
    }
    fragment.ReactiveOptionalStringAttribute(name, value) -> {
      let restore = Restore(name, current_attribute(node, name))
      case dataflow.value(dataflow.to_computation(value)) {
        Some(value) -> set_attribute(node, name, value)
        None -> Nil
      }
      #([restore], [])
    }
    fragment.ReactiveOptionalBooleanStringAttribute(name, value) -> {
      let restore = Restore(name, current_attribute(node, name))
      case dataflow.value(dataflow.to_computation(value)) {
        Some(value) -> set_attribute(node, name, bool_text(value))
        None -> Nil
      }
      #([restore], [])
    }
    fragment.ReactiveOptionalBooleanAttribute(name, value) -> {
      let restore = Restore(name, current_attribute(node, name))
      case dataflow.value(dataflow.to_computation(value)) {
        Some(True) -> set_attribute(node, name, "")
        Some(False) | None -> Nil
      }
      #([restore], [])
    }
    fragment.ReactiveOptionalClassAttribute(value) -> {
      let restore = Restore("class", current_attribute(node, "class"))
      case dataflow.value(dataflow.to_computation(value)) {
        Some(value) -> set_attribute(node, "class", join(" ", value))
        None -> Nil
      }
      #([restore], [])
    }
  }
}

fn set_portal_attributes_reactive(
  node: Node,
  attributes: List(fragment.Attribute),
  context: dataflow.Context,
  scheduler: markup_scheduler.Scheduler,
  style_binder: Option(StyleBinder),
) -> #(
  List(Restore),
  List(dataflow.Subscription),
  List(EventSubscription),
  List(fragment.OnAdd),
) {
  case attributes {
    [] -> #([], [], [], [])
    [first, ..rest] -> {
      let #(first_restores, first_subscriptions, first_events, first_on_adds) =
        set_portal_attribute_reactive(
          node,
          first,
          context,
          scheduler,
          style_binder,
        )
      let #(rest_restores, rest_subscriptions, rest_events, rest_on_adds) =
        set_portal_attributes_reactive(
          node,
          rest,
          context,
          scheduler,
          style_binder,
        )
      #(
        append(first_restores, rest_restores),
        append(first_subscriptions, rest_subscriptions),
        append(first_events, rest_events),
        append(first_on_adds, rest_on_adds),
      )
    }
  }
}

fn set_portal_attribute_reactive(
  node: Node,
  attribute: fragment.Attribute,
  context: dataflow.Context,
  scheduler: markup_scheduler.Scheduler,
  style_binder: Option(StyleBinder),
) -> #(
  List(Restore),
  List(dataflow.Subscription),
  List(EventSubscription),
  List(fragment.OnAdd),
) {
  case attribute {
    fragment.ReactiveOptionalStringAttribute(name, value) -> {
      let restore = Restore(name, current_attribute(node, name))
      let computation = dataflow.to_computation(value)
      let initial = dataflow.value(computation)
      case initial {
        Some(initial) -> set_attribute(node, name, initial)
        None -> Nil
      }
      let subscription =
        dataflow.effect_after_initial(
          fn(next_value) {
            set_optional_string_attribute(node, name, next_value)
          },
          computation,
        )
      #([restore], [subscription], [], [])
    }
    fragment.ReactiveOptionalBooleanStringAttribute(name, value) -> {
      let restore = Restore(name, current_attribute(node, name))
      let computation = dataflow.to_computation(value)
      let initial = dataflow.value(computation)
      case initial {
        Some(initial) -> set_attribute(node, name, bool_text(initial))
        None -> Nil
      }
      let subscription =
        dataflow.effect_after_initial(
          fn(next_value) {
            set_optional_bool_string_attribute(node, name, next_value)
          },
          computation,
        )
      #([restore], [subscription], [], [])
    }
    fragment.ReactiveOptionalBooleanAttribute(name, value) -> {
      let restore = Restore(name, current_attribute(node, name))
      let computation = dataflow.to_computation(value)
      let initial = dataflow.value(computation)
      case initial {
        Some(True) -> set_attribute(node, name, "")
        Some(False) | None -> Nil
      }
      let subscription =
        dataflow.effect_after_initial(
          fn(next_value) {
            set_optional_boolean_attribute(node, name, next_value)
          },
          computation,
        )
      #([restore], [subscription], [], [])
    }
    fragment.ReactiveOptionalClassAttribute(value) -> {
      let restore = Restore("class", current_attribute(node, "class"))
      let computation = dataflow.to_computation(value)
      let initial = dataflow.value(computation)
      case initial {
        Some(initial) -> set_attribute(node, "class", join(" ", initial))
        None -> Nil
      }
      let subscription =
        dataflow.effect_after_initial(
          fn(next_value) { set_optional_class_attribute(node, next_value) },
          computation,
        )
      #([restore], [subscription], [], [])
    }
    fragment.EventAttribute(name, listener) -> #(
      [],
      [],
      [
        add_event_listener(
          node,
          name,
          scheduled_transaction_listener(scheduler, context, listener),
        ),
      ],
      [],
    )
    fragment.StyleAttribute(value) -> {
      case style_binder {
        None -> {
          let #(restores, events) =
            set_portal_attribute(node, attribute, scheduler)
          #(restores, [], events, [])
        }
        Some(bind_style) -> {
          let restore = Restore("class", current_attribute(node, "class"))
          let subscription = bind_style(node, value)
          #([restore], [subscription], [], [])
        }
      }
    }
    fragment.OnAddAttribute(callback) -> #([], [], [], [callback])
    _ -> {
      let #(restores, events) = set_portal_attribute(node, attribute, scheduler)
      #(restores, [], events, [])
    }
  }
}

fn transaction_listener(
  context: dataflow.Context,
  listener: fn(fragment.Event) -> Nil,
) -> fn(fragment.Event) -> Nil {
  fn(event) {
    let _ =
      dataflow.txn(context, fn() {
        listener(event)
        Ok(Nil)
      })
    Nil
  }
}

fn scheduled_listener(
  scheduler: markup_scheduler.Scheduler,
  listener: fragment.EventListener,
) -> fn(fragment.Event) -> Nil {
  case listener {
    fragment.SyncEventListener(listener) -> fn(event) {
      markup_scheduler.run(scheduler, fn() { listener(event) })
      Nil
    }
    fragment.AsyncEventListener(listener) -> fn(event) {
      markup_scheduler.run_async_result(scheduler, fn(done) {
        listener(event)(done)
      })
      Nil
    }
  }
}

fn scheduled_transaction_listener(
  scheduler: markup_scheduler.Scheduler,
  context: dataflow.Context,
  listener: fragment.EventListener,
) -> fn(fragment.Event) -> Nil {
  case listener {
    fragment.SyncEventListener(listener) ->
      scheduled_listener(
        scheduler,
        fragment.SyncEventListener(transaction_listener(context, listener)),
      )
    fragment.AsyncEventListener(listener) -> fn(event) {
      markup_scheduler.run_async_result(scheduler, fn(done) {
        dataflow.txn_async(context, fn() { listener(event) })(done)
      })
      Nil
    }
  }
}

fn current_attribute(node: Node, name: String) -> Option(String) {
  case has_attribute(node, name) {
    True -> Some(get_attribute_value(node, name))
    False -> None
  }
}

fn restore_attributes(node: Node, restores: List(Restore)) -> Nil {
  case restores {
    [] -> Nil
    [Restore(name, value), ..rest] -> {
      case value {
        None -> remove_attribute(node, name)
        Some(value) -> set_attribute(node, name, value)
      }
      restore_attributes(node, rest)
    }
  }
}

fn run_cleanups(cleanups: List(fn() -> Nil)) -> Nil {
  case cleanups {
    [] -> Nil
    [first, ..rest] -> {
      first()
      run_cleanups(rest)
    }
  }
}

fn append(first: List(a), second: List(a)) -> List(a) {
  case first {
    [] -> second
    [head, ..tail] -> [head, ..append(tail, second)]
  }
}

fn join(separator: String, values: List(String)) -> String {
  case values {
    [] -> ""
    [first, ..rest] -> join_nonempty(separator, rest, first)
  }
}

fn join_nonempty(
  separator: String,
  values: List(String),
  output: String,
) -> String {
  case values {
    [] -> output
    [first, ..rest] ->
      join_nonempty(separator, rest, output <> separator <> first)
  }
}
