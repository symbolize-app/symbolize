import gleam/option.{type Option, None, Some}
import lib_dataflow as dataflow
import lib_error
import lib_markup_fragment as fragment
import lib_markup_scheduler as markup_scheduler

pub fn define(
  build: fn(fragment.ScopedContext, attrs) -> fragment.FragmentInput,
) -> fn(attrs) -> fragment.Fragment {
  fn(attributes) {
    fragment.dynamic(fn(mount_context) {
      mount_custom(mount_context, attributes, build)
    })
  }
}

// The source custom builder may resolve asynchronously. The returned
// fragment therefore owns an invisible range and fills it when the
// continuation resolves; this keeps the normal Fragment API usable by
// ranges and conditionals while preserving the source's delayed add/remove
// lifecycle.
pub fn define_async(
  build: fn(fragment.ScopedContext, attrs) ->
    lib_error.Async(fragment.FragmentInput, String),
) -> fn(attrs) -> fragment.Fragment {
  fn(attributes) {
    fragment.dynamic(fn(mount_context) {
      mount_async_custom(mount_context, attributes, build)
    })
  }
}

fn mount_custom(
  mount_context: fragment.MountContext,
  attributes: attrs,
  build: fn(fragment.ScopedContext, attrs) -> fragment.FragmentInput,
) -> fragment.MountedContent {
  let fragment.MountContext(
    parent: parent,
    runtime: runtime,
    markup: markup,
    scheduler: scheduler,
    mount: mount,
    ..,
  ) = mount_context
  let resource_context = dataflow.dataflow()
  let subscriptions = dataflow.state([])
  let cleanups = dataflow.state([])
  let scoped =
    fragment.ScopedContext(
      context: runtime,
      markup: markup,
      node: parent,
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
  let inner =
    mount(mount_context, fragment.to_fragment(build(scoped, attributes)))
  let fragment.MountedContent(
    nodes: nodes,
    subscriptions: inner_subscriptions,
    event_subscriptions: event_subscriptions,
    cleanups: inner_cleanups,
    pending: inner_pending,
    ..,
  ) = inner
  fragment.MountedContent(
    parent: parent,
    nodes: nodes,
    subscriptions: inner_subscriptions,
    event_subscriptions: event_subscriptions,
    cleanups: append(inner_cleanups, [
      fn() {
        run_custom_resources(
          dataflow.value(dataflow.to_computation(dataflow.mutation(cleanups))),
          dataflow.value(
            dataflow.to_computation(dataflow.mutation(subscriptions)),
          ),
        )
      },
    ]),
    scheduler: scheduler,
    pending: inner_pending,
  )
}

fn mount_async_custom(
  mount_context: fragment.MountContext,
  attributes: attrs,
  build: fn(fragment.ScopedContext, attrs) ->
    lib_error.Async(fragment.FragmentInput, String),
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
    mount: mount,
    remove: remove,
    create_comment: create_comment,
    append_node: append_node,
    insert_before: insert_before,
    remove_node: remove_node,
  ) = mount_context
  let resource_context = dataflow.dataflow()
  let subscriptions = dataflow.state([])
  let cleanups = dataflow.state([])
  let current = dataflow.state(None)
  let removed = dataflow.state(False)
  let scoped =
    fragment.ScopedContext(
      context: runtime,
      markup: markup,
      node: parent,
      scheduler: scheduler,
      subscribe: fn(subscription) {
        let assert Ok(Nil) =
          dataflow.txn(resource_context, fn() {
            let current_subscriptions =
              dataflow.value(
                dataflow.to_computation(dataflow.mutation(subscriptions)),
              )
            dataflow.set(resource_context, subscriptions, [
              subscription,
              ..current_subscriptions
            ])
          })
        Nil
      },
      defer: fn(cleanup) {
        let assert Ok(Nil) =
          dataflow.txn(resource_context, fn() {
            let current_cleanups =
              dataflow.value(
                dataflow.to_computation(dataflow.mutation(cleanups)),
              )
            dataflow.set(resource_context, cleanups, [
              cleanup,
              ..current_cleanups
            ])
          })
        Nil
      },
    )
  let start = create_comment(document, "")
  let end = create_comment(document, "")
  insert_or_append(parent, start, before, append_node, insert_before)
  insert_or_append(parent, end, before, append_node, insert_before)
  let child_context =
    fragment.MountContext(
      document: document,
      parent: parent,
      before: Some(end),
      runtime: runtime,
      markup: markup,
      scheduler: scheduler,
      style_binder: style_binder,
      deferred: deferred,
      mount: mount,
      remove: remove,
      create_comment: create_comment,
      append_node: append_node,
      insert_before: insert_before,
      remove_node: remove_node,
    )
  // Invoke the builder before returning from mount, matching the source's
  // async `add`: code before the first awaited operation (including scoped
  // resource registration) is synchronous.
  let operation = build(scoped, attributes)
  markup_scheduler.run_async_result(scheduler, fn(done) {
    operation(fn(result) {
      case result {
        Error(reason) -> done(Error(reason))
        Ok(value) -> {
          let assert Ok(Nil) =
            dataflow.txn(runtime, fn() {
              let is_removed =
                dataflow.value(
                  dataflow.to_computation(dataflow.mutation(removed)),
                )
              case is_removed {
                True -> Ok(Nil)
                False -> {
                  let mounted =
                    mount(child_context, fragment.to_fragment(value))
                  dataflow.set(runtime, current, Some(mounted))
                }
              }
            })
          done(Ok(Nil))
        }
      }
    })
  })
  let cleanup = fn() {
    let assert Ok(Nil) =
      dataflow.txn(runtime, fn() { dataflow.set(runtime, removed, True) })
    let mounted =
      dataflow.value(dataflow.to_computation(dataflow.mutation(current)))
    case mounted {
      None -> Nil
      Some(mounted) -> remove(mounted)
    }
    run_custom_resources(
      dataflow.value(dataflow.to_computation(dataflow.mutation(cleanups))),
      dataflow.value(dataflow.to_computation(dataflow.mutation(subscriptions))),
    )
  }
  fragment.MountedContent(
    parent: parent,
    nodes: [start, end],
    subscriptions: [],
    event_subscriptions: [],
    cleanups: [cleanup],
    scheduler: scheduler,
    pending: True,
  )
}

fn insert_or_append(
  parent: fragment.Node,
  node: fragment.Node,
  before: Option(fragment.Node),
  append_node: fn(fragment.Node, fragment.Node) -> Nil,
  insert_before: fn(fragment.Node, fragment.Node, fragment.Node) -> Nil,
) -> Nil {
  case before {
    None -> append_node(parent, node)
    Some(before) -> insert_before(parent, node, before)
  }
}

fn run_custom_resources(
  cleanups: List(fn() -> Nil),
  subscriptions: List(dataflow.Subscription),
) -> Nil {
  run_cleanups(cleanups)
  unsubscribe_all(subscriptions)
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

fn unsubscribe_all(subscriptions: List(dataflow.Subscription)) -> Nil {
  case subscriptions {
    [] -> Nil
    [first, ..rest] -> {
      dataflow.unsubscribe(first)
      unsubscribe_all(rest)
    }
  }
}

fn append(first: List(a), second: List(a)) -> List(a) {
  case first {
    [] -> second
    [head, ..tail] -> [head, ..append(tail, second)]
  }
}
