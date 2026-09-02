import gleam/option.{type Option, None, Some}
import lib_dataflow as dataflow
import lib_markup_dom as dom

pub opaque type MountedText {
  MountedText(
    parent: dom.Node,
    node: dom.Node,
    subscription: dataflow.Subscription,
  )
}

pub opaque type AttributeBinding {
  AttributeBinding(subscription: dataflow.Subscription)
}

pub opaque type EventBinding {
  EventBinding(subscription: dom.EventSubscription)
}

pub fn text(
  document: dom.Document,
  parent: dom.Node,
  content: dataflow.NodeOpt(String),
) -> MountedText {
  let node = dom.create_text_node(document, "")
  dom.append_node(parent, node)
  let computation = dataflow.to_computation(content)
  let subscription =
    dataflow.effect(
      fn(value) { dom.set_text_content(node, value) },
      computation,
    )
  MountedText(parent: parent, node: node, subscription: subscription)
}

pub fn node(mounted: MountedText) -> dom.Node {
  let MountedText(_, node, _) = mounted
  node
}

pub fn remove(mounted: MountedText) -> Nil {
  let MountedText(parent, node, subscription) = mounted
  dataflow.unsubscribe(subscription)
  dom.remove_node(parent, node)
}

pub fn attribute(
  node: dom.Node,
  name: String,
  value: dataflow.NodeOpt(String),
) -> AttributeBinding {
  let computation = dataflow.to_computation(value)
  let subscription =
    dataflow.effect(
      fn(value) { dom.set_attribute_value(node, name, value) },
      computation,
    )
  AttributeBinding(subscription: subscription)
}

pub fn unbind_attribute(binding: AttributeBinding) -> Nil {
  let AttributeBinding(subscription) = binding
  dataflow.unsubscribe(subscription)
}

pub fn optional_attribute(
  node: dom.Node,
  name: String,
  value: dataflow.NodeOpt(Option(String)),
) -> AttributeBinding {
  let computation = dataflow.to_computation(value)
  let subscription =
    dataflow.effect(
      fn(value) {
        case value {
          Some(value) -> dom.set_attribute_value(node, name, value)
          None -> dom.remove_attribute_value(node, name)
        }
      },
      computation,
    )
  AttributeBinding(subscription: subscription)
}

pub fn boolean_attribute(
  node: dom.Node,
  name: String,
  value: dataflow.NodeOpt(Bool),
) -> AttributeBinding {
  let computation = dataflow.to_computation(value)
  let subscription =
    dataflow.effect(
      fn(value) {
        case value {
          True -> dom.set_attribute_value(node, name, "")
          False -> dom.remove_attribute_value(node, name)
        }
      },
      computation,
    )
  AttributeBinding(subscription: subscription)
}

pub fn on(
  node: dom.Node,
  name: String,
  listener: fn(dom.Event) -> Nil,
) -> EventBinding {
  EventBinding(subscription: dom.add_event_listener(node, name, listener))
}

pub fn on_value(
  node: dom.Node,
  name: String,
  input: dataflow.NodeOpt(input),
  listener: fn(dom.Event, input) -> Nil,
) -> EventBinding {
  on(node, name, dataflow.handler(listener, input))
}

pub fn off(binding: EventBinding) -> Nil {
  let EventBinding(subscription) = binding
  dom.remove_event_listener(subscription)
}
