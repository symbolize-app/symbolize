import gleam/option.{type Option, None, Some}
import gleam/string
import lib_dataflow as dataflow
import lib_markup_dom as dom
import lib_markup_fragment as fragment
import lib_markup_style as markup_style

@external(javascript, "./each_ffi.mjs", "same_value_zero")
fn same_value_zero(left: a, right: a) -> Bool

pub opaque type Mounted(value, key) {
  Mounted(
    parent: dom.Node,
    start: dom.Node,
    end: dom.Node,
    current: dataflow.Mutation(List(Entry(value, key))),
    subscription: dataflow.Subscription,
  )
}

type Entry(value, key) {
  Entry(
    index: Int,
    key: key,
    value: value,
    state: dataflow.Mutation(value),
    mounted: dom.Mounted,
  )
}

pub fn mount(
  context: dataflow.Context,
  document: dom.Document,
  parent: dom.Node,
  items: dataflow.NodeOpt(List(value)),
  key: fn(value, Int) -> key,
  transform: fn(dataflow.Mutation(value)) -> fragment.Fragment,
) -> Mounted(value, key) {
  mount_inner(context, document, parent, items, key, transform, None)
}

pub fn each(
  transform: fn(dataflow.Mutation(value)) -> fragment.FragmentInput,
  key: fn(value, Int) -> key,
  items: dataflow.NodeOpt(List(value)),
) -> fragment.Fragment {
  fragment.dynamic(fn(mount_context) {
    mount_dynamic(mount_context, items, key, transform)
  })
}

type DynamicEntry(value, key) {
  DynamicEntry(
    index: Int,
    key: key,
    value: value,
    state: dataflow.Mutation(value),
    mounted: fragment.MountedContent,
  )
}

fn mount_dynamic(
  mount_context: fragment.MountContext,
  items: dataflow.NodeOpt(List(value)),
  key: fn(value, Int) -> key,
  transform: fn(dataflow.Mutation(value)) -> fragment.FragmentInput,
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
  let current = dataflow.state([])
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
      fn(items) {
        let old_entries =
          dataflow.value(dataflow.to_computation(dataflow.mutation(current)))
        let #(new_entries, rebuild) =
          reconcile_dynamic(
            context,
            mount,
            child_context,
            items,
            key,
            transform,
            old_entries,
            0,
            [],
            [],
            False,
          )
        remove_obsolete_dynamic_entries(remove, old_entries, new_entries)
        case rebuild {
          True -> move_dynamic_entries(insert_before, parent, end, new_entries)
          False -> Nil
        }
        let assert Ok(Nil) =
          dataflow.txn(context, fn() {
            dataflow.set(context, current, new_entries)
          })
        Nil
      },
      dataflow.to_computation(items),
    )
  let cleanup = fn() {
    let entries =
      dataflow.value(dataflow.to_computation(dataflow.mutation(current)))
    remove_dynamic_entries(remove, entries)
  }
  let pending =
    dynamic_entries_pending(
      dataflow.value(dataflow.to_computation(dataflow.mutation(current))),
    )
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

fn dynamic_entries_pending(entries: List(DynamicEntry(value, key))) -> Bool {
  case entries {
    [] -> False
    [first, ..rest] -> {
      let DynamicEntry(mounted: mounted, ..) = first
      fragment.mounted_pending(mounted) || dynamic_entries_pending(rest)
    }
  }
}

fn reconcile_dynamic(
  context: dataflow.Context,
  mount: fn(fragment.MountContext, fragment.Fragment) -> fragment.MountedContent,
  child_context: fragment.MountContext,
  items: List(value),
  key: fn(value, Int) -> key,
  transform: fn(dataflow.Mutation(value)) -> fragment.FragmentInput,
  old_entries: List(DynamicEntry(value, key)),
  index: Int,
  output: List(DynamicEntry(value, key)),
  seen_keys: List(key),
  rebuild: Bool,
) -> #(List(DynamicEntry(value, key)), Bool) {
  case items {
    [] -> #(reverse(output), rebuild || has_dynamic_entries(old_entries))
    [item, ..rest] -> {
      let item_key = key(item, index)
      case contains_key(item_key, seen_keys) {
        True -> {
          let message = "Duplicate key: " <> string.inspect(item_key)
          panic as message
        }
        False -> {
          let seen_keys = [item_key, ..seen_keys]
          case take_dynamic_entry(item_key, old_entries) {
            Some(#(entry, remaining)) -> {
              let DynamicEntry(old_index, _, _, state, mounted) = entry
              let assert Ok(Nil) =
                dataflow.txn(context, fn() {
                  dataflow.set(context, state, item)
                })
              let next = DynamicEntry(index, item_key, item, state, mounted)
              reconcile_dynamic(
                context,
                mount,
                child_context,
                rest,
                key,
                transform,
                remaining,
                index + 1,
                [next, ..output],
                seen_keys,
                rebuild || old_index != index,
              )
            }
            None -> {
              let state = dataflow.state(item)
              let mounted =
                mount(child_context, fragment.to_fragment(transform(state)))
              let next = DynamicEntry(index, item_key, item, state, mounted)
              reconcile_dynamic(
                context,
                mount,
                child_context,
                rest,
                key,
                transform,
                old_entries,
                index + 1,
                [next, ..output],
                seen_keys,
                True,
              )
            }
          }
        }
      }
    }
  }
}

fn take_dynamic_entry(
  wanted: key,
  entries: List(DynamicEntry(value, key)),
) -> Option(#(DynamicEntry(value, key), List(DynamicEntry(value, key)))) {
  case entries {
    [] -> None
    [first, ..rest] -> {
      let DynamicEntry(_, first_key, _, _, _) = first
      case same_value_zero(first_key, wanted) {
        True -> Some(#(first, rest))
        False ->
          case take_dynamic_entry(wanted, rest) {
            None -> None
            Some(#(entry, remaining)) -> Some(#(entry, [first, ..remaining]))
          }
      }
    }
  }
}

fn has_dynamic_entries(entries: List(a)) -> Bool {
  case entries {
    [] -> False
    _ -> True
  }
}

fn remove_dynamic_entries(
  remove: fn(fragment.MountedContent) -> Nil,
  entries: List(DynamicEntry(value, key)),
) -> Nil {
  case entries {
    [] -> Nil
    [first, ..rest] -> {
      let DynamicEntry(_, _, _, _, mounted) = first
      remove(mounted)
      remove_dynamic_entries(remove, rest)
    }
  }
}

fn remove_obsolete_dynamic_entries(
  remove: fn(fragment.MountedContent) -> Nil,
  old_entries: List(DynamicEntry(value, key)),
  new_entries: List(DynamicEntry(value, key)),
) -> Nil {
  case old_entries {
    [] -> Nil
    [first, ..rest] -> {
      let DynamicEntry(_, old_key, _, _, mounted) = first
      case contains_dynamic_entry_key(old_key, new_entries) {
        True -> Nil
        False -> remove(mounted)
      }
      remove_obsolete_dynamic_entries(remove, rest, new_entries)
    }
  }
}

fn contains_dynamic_entry_key(
  wanted: key,
  entries: List(DynamicEntry(value, key)),
) -> Bool {
  case entries {
    [] -> False
    [first, ..rest] -> {
      let DynamicEntry(_, first_key, _, _, _) = first
      same_value_zero(first_key, wanted)
      || contains_dynamic_entry_key(wanted, rest)
    }
  }
}

fn move_dynamic_entries(
  insert_before: fn(fragment.Node, fragment.Node, fragment.Node) -> Nil,
  parent: fragment.Node,
  before: fragment.Node,
  entries: List(DynamicEntry(value, key)),
) -> Nil {
  case entries {
    [] -> Nil
    [first, ..rest] -> {
      let DynamicEntry(_, _, _, _, mounted) = first
      let fragment.MountedContent(nodes: nodes, ..) = mounted
      move_nodes(insert_before, parent, nodes, before)
      move_dynamic_entries(insert_before, parent, before, rest)
    }
  }
}

fn move_nodes(
  insert_before: fn(fragment.Node, fragment.Node, fragment.Node) -> Nil,
  parent: fragment.Node,
  nodes: List(fragment.Node),
  before: fragment.Node,
) -> Nil {
  case nodes {
    [] -> Nil
    [first, ..rest] -> {
      insert_before(parent, first, before)
      move_nodes(insert_before, parent, rest, before)
    }
  }
}

pub fn mount_styled(
  context: dataflow.Context,
  document: dom.Document,
  parent: dom.Node,
  items: dataflow.NodeOpt(List(value)),
  key: fn(value, Int) -> key,
  transform: fn(dataflow.Mutation(value)) -> fragment.Fragment,
  style_host: markup_style.Host,
) -> Mounted(value, key) {
  mount_inner(
    context,
    document,
    parent,
    items,
    key,
    transform,
    Some(style_host),
  )
}

fn mount_inner(
  context: dataflow.Context,
  document: dom.Document,
  parent: dom.Node,
  items: dataflow.NodeOpt(List(value)),
  key: fn(value, Int) -> key,
  transform: fn(dataflow.Mutation(value)) -> fragment.Fragment,
  style_host: Option(markup_style.Host),
) -> Mounted(value, key) {
  let start = dom.create_comment(document, "")
  let end = dom.create_comment(document, "")
  dom.append_node(parent, start)
  dom.append_node(parent, end)
  let current = dataflow.state([])
  let subscription =
    dataflow.effect(
      fn(items) {
        let old_entries =
          dataflow.value(dataflow.to_computation(dataflow.mutation(current)))
        let #(new_entries, rebuild) =
          reconcile(
            context,
            document,
            parent,
            end,
            items,
            key,
            transform,
            style_host,
            old_entries,
            0,
            [],
            [],
            False,
          )
        remove_obsolete_entries(old_entries, new_entries)
        case rebuild {
          True -> move_entries_before(parent, end, new_entries)
          False -> Nil
        }
        let assert Ok(Nil) =
          dataflow.txn(context, fn() {
            dataflow.set(context, current, new_entries)
          })
        Nil
      },
      dataflow.to_computation(items),
    )
  Mounted(parent, start, end, current, subscription)
}

pub fn remove(mounted: Mounted(value, key)) -> Nil {
  let Mounted(parent, start, end, current, subscription) = mounted
  dataflow.unsubscribe(subscription)
  let entries =
    dataflow.value(dataflow.to_computation(dataflow.mutation(current)))
  remove_entries(entries)
  dom.remove_node(parent, start)
  dom.remove_node(parent, end)
}

fn reconcile(
  context: dataflow.Context,
  document: dom.Document,
  parent: dom.Node,
  end: dom.Node,
  items: List(value),
  key: fn(value, Int) -> key,
  transform: fn(dataflow.Mutation(value)) -> fragment.Fragment,
  style_host: Option(markup_style.Host),
  old_entries: List(Entry(value, key)),
  index: Int,
  output: List(Entry(value, key)),
  seen_keys: List(key),
  rebuild: Bool,
) -> #(List(Entry(value, key)), Bool) {
  case items {
    [] -> #(reverse(output), rebuild || has_entries(old_entries))
    [item, ..rest] -> {
      let item_key = key(item, index)
      case contains_key(item_key, seen_keys) {
        True -> {
          let message = "Duplicate key: " <> string.inspect(item_key)
          panic as message
        }
        False -> {
          let seen_keys = [item_key, ..seen_keys]
          case take_entry(item_key, old_entries) {
            Some(#(entry, remaining)) -> {
              let Entry(old_index, _, _, state, mounted) = entry
              let assert Ok(Nil) =
                dataflow.txn(context, fn() {
                  dataflow.set(context, state, item)
                })
              let next = Entry(index, item_key, item, state, mounted)
              reconcile(
                context,
                document,
                parent,
                end,
                rest,
                key,
                transform,
                style_host,
                remaining,
                index + 1,
                [next, ..output],
                seen_keys,
                rebuild || old_index != index,
              )
            }
            None -> {
              let state = dataflow.state(item)
              let mounted = case style_host {
                None ->
                  dom.mount_before_reactive(
                    context,
                    document,
                    parent,
                    end,
                    transform(state),
                  )
                Some(style_host) -> {
                  let #(_, mounted) =
                    markup_style.mount_before_reactive(
                      context,
                      style_host,
                      parent,
                      end,
                      transform(state),
                    )
                  mounted
                }
              }
              let next = Entry(index, item_key, item, state, mounted)
              reconcile(
                context,
                document,
                parent,
                end,
                rest,
                key,
                transform,
                style_host,
                old_entries,
                index + 1,
                [next, ..output],
                seen_keys,
                True,
              )
            }
          }
        }
      }
    }
  }
}

fn take_entry(
  wanted: key,
  entries: List(Entry(value, key)),
) -> Option(#(Entry(value, key), List(Entry(value, key)))) {
  case entries {
    [] -> None
    [first, ..rest] -> {
      let Entry(_, first_key, _, _, _) = first
      case same_value_zero(first_key, wanted) {
        True -> Some(#(first, rest))
        False ->
          case take_entry(wanted, rest) {
            None -> None
            Some(#(entry, remaining)) -> Some(#(entry, [first, ..remaining]))
          }
      }
    }
  }
}

fn contains_key(wanted: key, keys: List(key)) -> Bool {
  case keys {
    [] -> False
    [first, ..rest] ->
      same_value_zero(first, wanted) || contains_key(wanted, rest)
  }
}

fn has_entries(entries: List(a)) -> Bool {
  case entries {
    [] -> False
    _ -> True
  }
}

fn remove_entries(entries: List(Entry(value, key))) -> Nil {
  case entries {
    [] -> Nil
    [first, ..rest] -> {
      let Entry(_, _, _, _, mounted) = first
      dom.remove(mounted)
      remove_entries(rest)
    }
  }
}

fn remove_obsolete_entries(
  old_entries: List(Entry(value, key)),
  new_entries: List(Entry(value, key)),
) -> Nil {
  case old_entries {
    [] -> Nil
    [first, ..rest] -> {
      let Entry(_, old_key, _, _, mounted) = first
      case contains_entry_key(old_key, new_entries) {
        True -> Nil
        False -> dom.remove(mounted)
      }
      remove_obsolete_entries(rest, new_entries)
    }
  }
}

fn contains_entry_key(wanted: key, entries: List(Entry(value, key))) -> Bool {
  case entries {
    [] -> False
    [first, ..rest] -> {
      let Entry(_, first_key, _, _, _) = first
      same_value_zero(first_key, wanted) || contains_entry_key(wanted, rest)
    }
  }
}

fn move_entries_before(
  parent: dom.Node,
  before: dom.Node,
  entries: List(Entry(value, key)),
) -> Nil {
  case entries {
    [] -> Nil
    [first, ..rest] -> {
      let Entry(_, _, _, _, mounted) = first
      dom.move_before(parent, dom.nodes(mounted), before)
      move_entries_before(parent, before, rest)
    }
  }
}

fn reverse(values: List(a)) -> List(a) {
  reverse_loop(values, [])
}

fn reverse_loop(values: List(a), output: List(a)) -> List(a) {
  case values {
    [] -> output
    [first, ..rest] -> reverse_loop(rest, [first, ..output])
  }
}
