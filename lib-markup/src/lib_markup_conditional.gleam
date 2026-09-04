import gleam/option.{type Option, None, Some}
import lib_dataflow as dataflow
import lib_markup_dom as dom
import lib_markup_fragment as fragment
import lib_markup_style as markup_style

type DynamicMount(value) {
  TruthyMount(dataflow.Mutation(value), fragment.MountedContent)
  FalsyMount(fragment.MountedContent)
}

pub opaque type Mounted {
  Mounted(
    parent: dom.Node,
    start: dom.Node,
    end: dom.Node,
    current: dataflow.Mutation(Option(dom.Mounted)),
    subscription: dataflow.Subscription,
  )
}

pub fn mount(
  context: dataflow.Context,
  document: dom.Document,
  parent: dom.Node,
  condition: dataflow.NodeOpt(Bool),
  when_true: fragment.Fragment,
  when_false: fragment.Fragment,
) -> Mounted {
  let start = dom.create_comment(document, "")
  let end = dom.create_comment(document, "")
  dom.append_node(parent, start)
  dom.append_node(parent, end)
  let current = dataflow.state(None)
  let subscription =
    dataflow.effect(
      fn(show) {
        let assert Ok(Nil) =
          dataflow.txn(context, fn() {
            let old_mount =
              dataflow.value(
                dataflow.to_computation(dataflow.mutation(current)),
              )
            remove_mount(old_mount)
            let new_mount = case show {
              True ->
                dom.mount_before_reactive(
                  context,
                  document,
                  parent,
                  end,
                  when_true,
                )
              False ->
                dom.mount_before_reactive(
                  context,
                  document,
                  parent,
                  end,
                  when_false,
                )
            }
            dataflow.set(context, current, Some(new_mount))
          })
        Nil
      },
      dataflow.to_computation(condition),
    )
  Mounted(parent, start, end, current, subscription)
}

pub fn if_(
  condition: dataflow.NodeOpt(value),
  is_truthy: fn(value) -> Bool,
  when_true: fn(dataflow.Computation(value)) -> fragment.FragmentInput,
  when_false: fn() -> fragment.FragmentInput,
) -> fragment.Fragment {
  fragment.dynamic(fn(mount_context) {
    mount_dynamic(
      mount_context,
      condition,
      fn(value) {
        case is_truthy(value) {
          True -> Some(value)
          False -> None
        }
      },
      when_true,
      when_false,
    )
  })
}

// The source's conditional type removes null and undefined from the true
// branch. Option is the direct Gleam representation for that case, so this
// specialization passes the unwrapped value to the true branch instead of
// requiring every caller to pattern match the original Option again.
pub fn if_some(
  condition: dataflow.NodeOpt(Option(value)),
  when_true: fn(dataflow.Computation(value)) -> fragment.FragmentInput,
  when_false: fn() -> fragment.FragmentInput,
) -> fragment.Fragment {
  fragment.dynamic(fn(mount_context) {
    mount_dynamic(
      mount_context,
      condition,
      fn(value) { value },
      when_true,
      when_false,
    )
  })
}

pub fn if_bool(
  condition: dataflow.NodeOpt(Bool),
  when_true: fn(dataflow.Computation(Bool)) -> fragment.FragmentInput,
  when_false: fn() -> fragment.FragmentInput,
) -> fragment.Fragment {
  if_(condition, fn(value) { value }, when_true, when_false)
}

fn mount_dynamic(
  mount_context: fragment.MountContext,
  condition: dataflow.NodeOpt(value),
  truthy_value: fn(value) -> Option(truthy),
  when_true: fn(dataflow.Computation(truthy)) -> fragment.FragmentInput,
  when_false: fn() -> fragment.FragmentInput,
) -> fragment.MountedContent {
  let fragment.MountContext(
    document: document,
    parent: parent,
    runtime: context,
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
    ..,
  ) = mount_context
  let start = create_comment(document, "")
  let end = create_comment(document, "")
  append_node(parent, start)
  append_node(parent, end)
  let current = dataflow.state(None)
  let child_context =
    fragment.MountContext(
      document: document,
      parent: parent,
      before: Some(end),
      runtime: context,
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
  let subscription =
    dataflow.effect(
      fn(show) {
        let assert Ok(Nil) =
          dataflow.txn(context, fn() {
            let old_mount =
              dataflow.value(
                dataflow.to_computation(dataflow.mutation(current)),
              )
            let next = case truthy_value(show), old_mount {
              Some(truthy), Some(TruthyMount(state, _mounted)) -> {
                let assert Ok(Nil) = dataflow.set(context, state, truthy)
                old_mount
              }
              Some(truthy), _ -> {
                remove_dynamic_mount(remove, old_mount)
                let state = dataflow.state(truthy)
                let condition =
                  dataflow.to_computation(dataflow.mutation(state))
                let mounted =
                  mount(
                    child_context,
                    fragment.to_fragment(when_true(condition)),
                  )
                Some(TruthyMount(state, mounted))
              }
              None, Some(FalsyMount(_mounted)) -> old_mount
              None, _ -> {
                remove_dynamic_mount(remove, old_mount)
                let mounted =
                  mount(child_context, fragment.to_fragment(when_false()))
                Some(FalsyMount(mounted))
              }
            }
            dataflow.set(context, current, next)
          })
        Nil
      },
      dataflow.to_computation(condition),
    )
  let cleanup = fn() {
    let old_mount =
      dataflow.value(dataflow.to_computation(dataflow.mutation(current)))
    remove_dynamic_mount(remove, old_mount)
  }
  let pending = case
    dataflow.value(dataflow.to_computation(dataflow.mutation(current)))
  {
    None -> False
    Some(TruthyMount(_, mounted)) -> fragment.mounted_pending(mounted)
    Some(FalsyMount(mounted)) -> fragment.mounted_pending(mounted)
  }
  fragment.MountedContent(
    parent: parent,
    nodes: [start, end],
    subscriptions: [subscription],
    event_subscriptions: [],
    cleanups: [cleanup],
    scheduler: scheduler,
    pending: pending,
  )
}

pub fn mount_styled(
  context: dataflow.Context,
  document: dom.Document,
  parent: dom.Node,
  condition: dataflow.NodeOpt(Bool),
  when_true: fragment.Fragment,
  when_false: fragment.Fragment,
  style_host: markup_style.Host,
) -> Mounted {
  let start = dom.create_comment(document, "")
  let end = dom.create_comment(document, "")
  dom.append_node(parent, start)
  dom.append_node(parent, end)
  let current = dataflow.state(None)
  let subscription =
    dataflow.effect(
      fn(show) {
        let assert Ok(Nil) =
          dataflow.txn(context, fn() {
            let old_mount =
              dataflow.value(
                dataflow.to_computation(dataflow.mutation(current)),
              )
            remove_mount(old_mount)
            let new_mount = case show {
              True -> {
                let #(_, mounted) =
                  markup_style.mount_before_reactive(
                    context,
                    style_host,
                    parent,
                    end,
                    when_true,
                  )
                mounted
              }
              False -> {
                let #(_, mounted) =
                  markup_style.mount_before_reactive(
                    context,
                    style_host,
                    parent,
                    end,
                    when_false,
                  )
                mounted
              }
            }
            dataflow.set(context, current, Some(new_mount))
          })
        Nil
      },
      dataflow.to_computation(condition),
    )
  Mounted(parent, start, end, current, subscription)
}

pub fn remove(mounted: Mounted) -> Nil {
  let Mounted(parent, start, end, current, subscription) = mounted
  dataflow.unsubscribe(subscription)
  let old_mount =
    dataflow.value(dataflow.to_computation(dataflow.mutation(current)))
  remove_mount(old_mount)
  dom.remove_node(parent, start)
  dom.remove_node(parent, end)
}

fn remove_mount(mounted: Option(dom.Mounted)) -> Nil {
  case mounted {
    None -> Nil
    Some(mounted) -> dom.remove(mounted)
  }
}

fn remove_dynamic_mount(
  remove: fn(fragment.MountedContent) -> Nil,
  mounted: Option(DynamicMount(value)),
) -> Nil {
  case mounted {
    None -> Nil
    Some(TruthyMount(_, mounted)) -> remove(mounted)
    Some(FalsyMount(mounted)) -> remove(mounted)
  }
}
