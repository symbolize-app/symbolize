import gleam/option.{type Option}
import lib_dataflow as dataflow
import lib_error
import lib_markup_scheduler as markup_scheduler
import lib_styling as styling

pub type Document

pub type Node

// A style layer is a browser-owned CSS rule container. It lives here rather
// than in the DOM module so markup contexts can carry it without creating a
// module cycle.
pub type StyleLayer

pub type Event

// These are distinct foreign boundary types because the source API exposes
// the corresponding DOM event subclasses in listener callback signatures.
// They intentionally have no Gleam constructors: the browser supplies the
// values, and event property accessors belong at the same FFI boundary.
pub type UIEvent

pub type AnimationEvent

pub type MouseEvent

pub type InputEvent

pub type FocusEvent

pub type ClipboardEvent

pub type CompositionEvent

pub type DragEvent

pub type ErrorEvent

pub type FormDataEvent

pub type PointerEvent

pub type KeyboardEvent

pub type ProgressEvent

pub type TouchEvent

pub type TransitionEvent

pub type WheelEvent

pub type GamepadEvent

pub type HashChangeEvent

pub type MessageEvent

pub type PageTransitionEvent

pub type PopStateEvent

pub type PromiseRejectionEvent

pub type SecurityPolicyViolationEvent

pub type StorageEvent

pub type SubmitEvent

@external(javascript, "./dom_ffi.mjs", "mouse_event_button")
pub fn mouse_event_button(event: MouseEvent) -> Int

@external(javascript, "./dom_ffi.mjs", "keyboard_event_key")
pub fn keyboard_event_key(event: KeyboardEvent) -> String

pub type EventSubscription

pub type StyleBinder =
  fn(Node, dataflow.NodeOpt(styling.AtomOpt)) -> dataflow.Subscription

pub type MarkupContext {
  MarkupContext(
    document: Document,
    scheduler: markup_scheduler.Scheduler,
    style_layer: Option(StyleLayer),
  )
}

pub type ScopedContext {
  ScopedContext(
    context: dataflow.Context,
    markup: MarkupContext,
    node: Node,
    scheduler: markup_scheduler.Scheduler,
    subscribe: fn(dataflow.Subscription) -> Nil,
    defer: fn(fn() -> Nil) -> Nil,
  )
}

pub type OnAdd {
  SyncOnAdd(fn(OnAddEvent) -> Nil)
  AsyncOnAdd(fn(OnAddEvent) -> lib_error.Async(Nil, String))
}

pub fn sync_on_add(callback: fn(OnAddEvent) -> Nil) -> OnAdd {
  SyncOnAdd(callback)
}

pub fn async_on_add(
  callback: fn(OnAddEvent) -> lib_error.Async(Nil, String),
) -> OnAdd {
  AsyncOnAdd(callback)
}

// Source event attributes accept either a synchronous listener or a Promise
// returning listener. Gleam has no union of function return types, so the
// distinction is explicit at this boundary while the event payload remains
// the common browser Event type used by the DOM fragment.
pub type EventListener {
  SyncEventListener(fn(Event) -> Nil)
  AsyncEventListener(fn(Event) -> lib_error.Async(Nil, String))
}

pub type OnAddEvent {
  OnAddEvent(ctx: ScopedContext, element: Node)
}

pub type MountedContent {
  MountedContent(
    parent: Node,
    nodes: List(Node),
    subscriptions: List(dataflow.Subscription),
    event_subscriptions: List(EventSubscription),
    cleanups: List(fn() -> Nil),
    scheduler: markup_scheduler.Scheduler,
    pending: Bool,
  )
}

pub fn mounted_pending(mounted: MountedContent) -> Bool {
  let MountedContent(pending: pending, ..) = mounted
  pending
}

pub type MountContext {
  MountContext(
    document: Document,
    parent: Node,
    before: Option(Node),
    runtime: dataflow.Context,
    markup: MarkupContext,
    scheduler: markup_scheduler.Scheduler,
    style_binder: Option(StyleBinder),
    deferred: Bool,
    mount: fn(MountContext, Fragment) -> MountedContent,
    remove: fn(MountedContent) -> Nil,
    create_comment: fn(Document, String) -> Node,
    append_node: fn(Node, Node) -> Nil,
    insert_before: fn(Node, Node, Node) -> Nil,
    remove_node: fn(Node, Node) -> Nil,
  )
}

pub type Dynamic {
  Dynamic(mount: fn(MountContext) -> MountedContent)
}

pub type Fragment {
  Empty
  Text(String)
  ReactiveTextNode(dataflow.NodeOpt(String))
  Range(List(Fragment))
  ElementNode(
    namespace: Namespace,
    tag: String,
    attributes: List(Attribute),
    content: Fragment,
  )
  PortalNode(node: Node, attributes: List(Attribute), content: Fragment)
  DynamicNode(Dynamic)
}

pub type Namespace {
  Html
  Svg
  MathMl
}

pub type Attribute {
  StringAttribute(name: String, value: String)
  BooleanAttribute(name: String, value: Bool)
  ClassAttribute(List(String))
  StyleClassAttribute(List(String))
  OnAddAttribute(OnAdd)
  EventAttribute(name: String, listener: EventListener)
  StyleAttribute(value: dataflow.NodeOpt(styling.AtomOpt))
  ReactiveOptionalStringAttribute(
    name: String,
    value: dataflow.NodeOpt(Option(String)),
  )
  ReactiveOptionalBooleanStringAttribute(
    name: String,
    value: dataflow.NodeOpt(Option(Bool)),
  )
  ReactiveOptionalBooleanAttribute(
    name: String,
    value: dataflow.NodeOpt(Option(Bool)),
  )
  ReactiveOptionalClassAttribute(value: dataflow.NodeOpt(Option(List(String))))
}

pub type FragmentInput {
  TextInput(String)
  ReactiveTextInput(dataflow.NodeOpt(String))
  FragmentInput(Fragment)
  ListInput(List(FragmentInput))
  EmptyInput
}

pub fn empty() -> Fragment {
  Empty
}

pub fn text(value: String) -> Fragment {
  Text(value)
}

pub fn reactive_text(value: dataflow.NodeOpt(String)) -> Fragment {
  ReactiveTextNode(value)
}

pub fn dynamic(mount: fn(MountContext) -> MountedContent) -> Fragment {
  DynamicNode(Dynamic(mount))
}

pub fn markup_context(
  document: Document,
  scheduler: markup_scheduler.Scheduler,
  style_layer: Option(StyleLayer),
) -> MarkupContext {
  MarkupContext(
    document: document,
    scheduler: scheduler,
    style_layer: style_layer,
  )
}

pub fn markup_document(context: MarkupContext) -> Document {
  let MarkupContext(document: document, ..) = context
  document
}

pub fn markup_scheduler(context: MarkupContext) -> markup_scheduler.Scheduler {
  let MarkupContext(scheduler: scheduler, ..) = context
  scheduler
}

pub fn markup_style_layer(context: MarkupContext) -> Option(StyleLayer) {
  let MarkupContext(style_layer: style_layer, ..) = context
  style_layer
}

pub fn scoped_markup(context: ScopedContext) -> MarkupContext {
  let ScopedContext(markup: markup, ..) = context
  markup
}

pub fn scoped_document(context: ScopedContext) -> Document {
  markup_context_document(scoped_markup(context))
}

pub fn scoped_scheduler(context: ScopedContext) -> markup_scheduler.Scheduler {
  markup_context_scheduler(scoped_markup(context))
}

fn markup_context_document(context: MarkupContext) -> Document {
  let MarkupContext(document: document, ..) = context
  document
}

fn markup_context_scheduler(
  context: MarkupContext,
) -> markup_scheduler.Scheduler {
  let MarkupContext(scheduler: scheduler, ..) = context
  scheduler
}

pub fn scoped_effect(
  context: ScopedContext,
  callback: fn(value) -> Nil,
  computation: dataflow.Computation(value),
) -> Nil {
  let ScopedContext(subscribe: subscribe, ..) = context
  subscribe(dataflow.effect(callback, computation))
}

// The source scoped effect awaits the initial callback before installing its
// subscription. Gleam has no native Promise type, so this keeps that
// ordering explicit as a continuation. Later updates are serialized through
// the same scheduler used by DOM event work.
pub fn scoped_effect_async(
  context: ScopedContext,
  callback: fn(value) -> lib_error.Async(Nil, reason),
  computation: dataflow.Computation(value),
) -> lib_error.Async(Nil, reason) {
  let ScopedContext(scheduler: scheduler, subscribe: subscribe, ..) = context
  fn(done) {
    callback(dataflow.value(computation))(fn(initial_result) {
      case initial_result {
        Error(reason) -> done(Error(reason))
        Ok(_) -> {
          let subscription =
            dataflow.effect_after_initial(
              fn(value) {
                markup_scheduler.run_async(scheduler, fn(effect_done) {
                  callback(value)(fn(result) {
                    case result {
                      Ok(_) -> effect_done()
                      Error(_) -> panic as "scoped effect callback failed"
                    }
                  })
                })
              },
              computation,
            )
          subscribe(subscription)
          done(Ok(Nil))
        }
      }
    })
  }
}

pub fn scoped_effect_async2(
  context: ScopedContext,
  callback: fn(first, second) -> lib_error.Async(Nil, reason),
  first: dataflow.Computation(first),
  second: dataflow.Computation(second),
) -> lib_error.Async(Nil, reason) {
  let ScopedContext(scheduler: scheduler, subscribe: subscribe, ..) = context
  fn(done) {
    callback(dataflow.value(first), dataflow.value(second))(fn(result) {
      case result {
        Error(reason) -> done(Error(reason))
        Ok(_) -> {
          let subscription =
            dataflow.effect2_after_initial(
              fn(first, second) {
                markup_scheduler.run_async(scheduler, fn(effect_done) {
                  callback(first, second)(fn(result) {
                    case result {
                      Ok(_) -> effect_done()
                      Error(_) -> panic as "scoped effect callback failed"
                    }
                  })
                })
              },
              first,
              second,
            )
          subscribe(subscription)
          done(Ok(Nil))
        }
      }
    })
  }
}

pub fn scoped_effect_async3(
  context: ScopedContext,
  callback: fn(first, second, third) -> lib_error.Async(Nil, reason),
  first: dataflow.Computation(first),
  second: dataflow.Computation(second),
  third: dataflow.Computation(third),
) -> lib_error.Async(Nil, reason) {
  let ScopedContext(scheduler: scheduler, subscribe: subscribe, ..) = context
  fn(done) {
    callback(
      dataflow.value(first),
      dataflow.value(second),
      dataflow.value(third),
    )(fn(result) {
      case result {
        Error(reason) -> done(Error(reason))
        Ok(_) -> {
          let subscription =
            dataflow.effect3_after_initial(
              fn(first, second, third) {
                markup_scheduler.run_async(scheduler, fn(effect_done) {
                  callback(first, second, third)(fn(result) {
                    case result {
                      Ok(_) -> effect_done()
                      Error(_) -> panic as "scoped effect callback failed"
                    }
                  })
                })
              },
              first,
              second,
              third,
            )
          subscribe(subscription)
          done(Ok(Nil))
        }
      }
    })
  }
}

pub fn scoped_node(context: ScopedContext) -> Node {
  let ScopedContext(node: node, ..) = context
  node
}

pub fn scoped_dataflow(context: ScopedContext) -> dataflow.Context {
  let ScopedContext(context: context, ..) = context
  context
}

pub fn scoped_defer(context: ScopedContext, callback: fn() -> Nil) -> Nil {
  let ScopedContext(defer: defer, ..) = context
  defer(callback)
}

pub fn range(values: List(FragmentInput)) -> Fragment {
  values
  |> list_map(to_fragment)
  |> Range
}

pub fn element(
  tag: String,
  attributes: List(Attribute),
  content: FragmentInput,
) -> Fragment {
  element_in(Html, tag, attributes, content)
}

pub fn element_in(
  namespace: Namespace,
  tag: String,
  attributes: List(Attribute),
  content: FragmentInput,
) -> Fragment {
  ElementNode(namespace, tag, attributes, to_fragment(content))
}

pub fn to_fragment(value: FragmentInput) -> Fragment {
  case value {
    EmptyInput -> Empty
    TextInput(value) -> Text(value)
    ReactiveTextInput(value) -> ReactiveTextNode(value)
    FragmentInput(value) -> value
    ListInput(values) -> range(values)
  }
}

pub fn text_content(fragment: Fragment) -> String {
  case fragment {
    Empty -> ""
    Text(value) -> value
    ReactiveTextNode(value) -> dataflow.value(dataflow.to_computation(value))
    Range(values) -> join("", list_map(values, text_content))
    ElementNode(_, _, _, content) -> text_content(content)
    PortalNode(_, _, content) -> text_content(content)
    DynamicNode(_) -> ""
  }
}

fn list_map(values: List(a), transform: fn(a) -> b) -> List(b) {
  case values {
    [] -> []
    [first, ..rest] -> [transform(first), ..list_map(rest, transform)]
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
