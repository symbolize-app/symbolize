import gleam/io
import gleam/list
import lib_dataflow as dataflow

type History(value) {
  History(context: dataflow.Context, state: dataflow.Mutation(List(value)))
}

fn new_history() -> History(value) {
  History(dataflow.dataflow(), dataflow.state([]))
}

fn push_history(history: History(value), value: value) -> Nil {
  let History(context, state) = history
  let current =
    dataflow.value(dataflow.to_computation(dataflow.mutation(state)))
  let assert Ok(Nil) =
    dataflow.txn(context, fn() {
      dataflow.set(context, state, [value, ..current])
    })
  Nil
}

@external(javascript, "./dataflow_test_ffi.mjs", "schedule_delay")
fn schedule_delay(milliseconds: Int, callback: fn() -> Nil) -> Nil

fn clear_history(history: History(value)) -> Nil {
  let History(context, state) = history
  let assert Ok(Nil) =
    dataflow.txn(context, fn() { dataflow.set(context, state, []) })
  Nil
}

fn delay(
  counter: History(Nil),
  milliseconds: Int,
  callback: fn() -> Nil,
) -> Nil {
  push_history(counter, Nil)
  schedule_delay(milliseconds, callback)
}

type Point {
  Point(x: Int, y: Int)
}

pub fn run(done: fn() -> Nil) {
  pure_and_map()
  map_cache()
  nested_map_value_cache()
  mapped_output_suppression()
  state_and_transaction()
  transaction_commit_order()
  nested_transaction()
  nested_rollback()
  rollback_transaction()
  derived_state()
  effects_and_unsubscribe()
  effect_after_initial()
  map2_and_atomic_effect()
  map2_tracks_each_dependency()
  map3_tracks_each_dependency()
  effect_arities()
  partial_graph_effect()
  handler2_values()
  map3_values()
  derived2_state()
  explicit_field_mapping()
  io.println("lib-dataflow explicit Gleam parity slice passed")
  let delay_counter = new_history()
  async_computations(delay_counter)
  async_multiple_values(delay_counter)
  async_map_epoch_cache(delay_counter)
  async_map_multi_epoch_cache(delay_counter)
  async_map_state_epoch(delay_counter)
  async_map_error_persists()
  async_effect_error_persists()
  async_state_and_effect()
  async_errors_propagate_through_transactions()
  async_derived_and_effect_multiple()
  async_nested_transaction_epoch(delay_counter, fn() {
    async_derived_in_progress_epoch(delay_counter, fn() {
      async_derived2_in_progress_epoch(delay_counter, fn() {
        async_map_queued_epochs(delay_counter, done)
      })
    })
  })
}

pub fn main() {
  run(fn() { Nil })
}

fn pure_and_map() {
  let parent = dataflow.pure(2)
  let child = dataflow.map(fn(value) { value + 1 }, dataflow.reactive(parent))
  assert dataflow.value(parent) == 2
  assert dataflow.value(child) == 3
}

fn map_cache() {
  let parent = dataflow.pure(2)
  let history = new_history()
  let child =
    dataflow.map(
      fn(value) {
        push_history(history, value)
        value + 1
      },
      dataflow.reactive(parent),
    )
  assert history_values(history) == []
  assert dataflow.value(child) == 3
  assert history_values(history) == [2]
  assert dataflow.value(child) == 3
  assert history_values(history) == [2]
}

fn nested_map_value_cache() {
  let context = dataflow.dataflow()
  let parent = dataflow.state(0)
  let inner = dataflow.map(fn(_value) { "same" }, dataflow.mutation(parent))
  let history = new_history()
  let outer =
    dataflow.map(
      fn(value) {
        push_history(history, value)
        value <> "!"
      },
      dataflow.reactive(inner),
    )

  assert dataflow.value(outer) == "same!"
  assert history_values(history) == ["same"]
  let assert Ok(Nil) =
    dataflow.txn(context, fn() { dataflow.set(context, parent, 1) })
  assert dataflow.value(outer) == "same!"
  assert history_values(history) == ["same"]
}

fn mapped_output_suppression() {
  let context = dataflow.dataflow()
  let state = dataflow.state(0)
  let child = dataflow.map(fn(_value) { "same" }, dataflow.mutation(state))
  let history = new_history()
  let subscription =
    dataflow.effect(fn(value) { push_history(history, value) }, child)
  assert history_values(history) == ["same"]
  let assert Ok(Nil) =
    dataflow.txn(context, fn() { dataflow.set(context, state, 1) })
  assert history_values(history) == ["same"]
  dataflow.unsubscribe(subscription)
}

fn state_and_transaction() {
  let context = dataflow.dataflow()
  let state = dataflow.state("x")
  assert dataflow.value(dataflow.to_computation(dataflow.mutation(state)))
    == "x"
  let assert Ok(Nil) =
    dataflow.txn(context, fn() { dataflow.set(context, state, "a") })
  assert dataflow.value(dataflow.to_computation(dataflow.mutation(state)))
    == "a"
  assert dataflow.set(context, state, "b")
    == Error(dataflow.TransactionRequired)
}

fn transaction_commit_order() {
  let context = dataflow.dataflow()
  let first = dataflow.state(0)
  let second = dataflow.state(0)
  let history = new_history()
  let first_subscription =
    dataflow.effect(
      fn(value) { push_history(history, #("first", value)) },
      dataflow.to_computation(dataflow.mutation(first)),
    )
  let second_subscription =
    dataflow.effect(
      fn(value) { push_history(history, #("second", value)) },
      dataflow.to_computation(dataflow.mutation(second)),
    )
  let assert Ok(Nil) =
    dataflow.txn(context, fn() {
      let assert Ok(Nil) = dataflow.set(context, first, 1)
      dataflow.set(context, second, 2)
    })
  assert history_values(history)
    == [#("first", 0), #("second", 0), #("first", 1), #("second", 2)]
  dataflow.unsubscribe(first_subscription)
  dataflow.unsubscribe(second_subscription)
}

fn nested_transaction() {
  let context = dataflow.dataflow()
  let first = dataflow.state("x")
  let second = dataflow.state("y")
  let assert Ok(Nil) =
    dataflow.txn(context, fn() {
      let assert Ok(Nil) = dataflow.set(context, first, "a")
      let assert Ok(Nil) =
        dataflow.txn(context, fn() { dataflow.set(context, second, "b") })
      Ok(Nil)
    })
  assert dataflow.value(dataflow.to_computation(dataflow.mutation(first)))
    == "a"
  assert dataflow.value(dataflow.to_computation(dataflow.mutation(second)))
    == "b"
}

fn nested_rollback() {
  let context = dataflow.dataflow()
  let first = dataflow.state("x")
  let second = dataflow.state("y")
  let assert Ok(Nil) =
    dataflow.txn(context, fn() {
      let assert Ok(Nil) = dataflow.set(context, first, "a")
      assert dataflow.txn(context, fn() {
          let assert Ok(Nil) = dataflow.set(context, second, "b")
          Error("rollback")
        })
        == Error("rollback")
      Ok(Nil)
    })
  assert dataflow.value(dataflow.to_computation(dataflow.mutation(first)))
    == "a"
  assert dataflow.value(dataflow.to_computation(dataflow.mutation(second)))
    == "y"
}

fn rollback_transaction() {
  let context = dataflow.dataflow()
  let state = dataflow.state("x")
  assert dataflow.txn(context, fn() {
      let assert Ok(Nil) = dataflow.set(context, state, "y")
      Error("rollback")
    })
    == Error("rollback")
  assert dataflow.value(dataflow.to_computation(dataflow.mutation(state)))
    == "x"
}

fn derived_state() {
  let context = dataflow.dataflow()
  let source = dataflow.state(Point(1, 2))
  let x =
    dataflow.derived(
      fn(point: Point) { point.x },
      fn(context, new_x, point: Point) {
        dataflow.set(context, source, Point(new_x, point.y))
      },
      dataflow.mutation(source),
    )
  let assert Ok(Nil) =
    dataflow.txn(context, fn() { dataflow.set(context, x, 5) })
  assert dataflow.value(dataflow.to_computation(dataflow.mutation(source)))
    == Point(5, 2)
  assert dataflow.value(dataflow.to_computation(dataflow.mutation(x))) == 5
}

fn effects_and_unsubscribe() {
  let context = dataflow.dataflow()
  let state = dataflow.state(0)
  let child = dataflow.map(fn(value) { value * 2 }, dataflow.mutation(state))
  let subscription =
    dataflow.effect(
      fn(value) {
        assert value == 0 || value == 2
      },
      child,
    )
  let assert Ok(Nil) =
    dataflow.txn(context, fn() { dataflow.set(context, state, 1) })
  assert dataflow.value(dataflow.to_computation(dataflow.mutation(state))) == 1
  dataflow.unsubscribe(subscription)
  let assert Ok(Nil) =
    dataflow.txn(context, fn() { dataflow.set(context, state, 2) })
  Nil
}

fn effect_after_initial() {
  let context = dataflow.dataflow()
  let state = dataflow.state(0)
  let history = new_history()
  let subscription =
    dataflow.effect_after_initial(
      fn(value) { push_history(history, value) },
      dataflow.to_computation(dataflow.mutation(state)),
    )
  assert history_values(history) == []
  let assert Ok(Nil) =
    dataflow.txn(context, fn() { dataflow.set(context, state, 1) })
  assert history_values(history) == [1]
  dataflow.unsubscribe(subscription)
}

fn map2_and_atomic_effect() {
  let context = dataflow.dataflow()
  let first = dataflow.state(1)
  let second = dataflow.state(2)
  let total =
    dataflow.map2(
      fn(first, second) { first + second },
      dataflow.mutation(first),
      dataflow.mutation(second),
    )
  let history = new_history()
  let subscription =
    dataflow.effect(fn(value) { push_history(history, value) }, total)
  assert history_values(history) == [3]
  let assert Ok(Nil) =
    dataflow.txn(context, fn() {
      let assert Ok(Nil) = dataflow.set(context, first, 4)
      dataflow.set(context, second, 3)
    })
  assert dataflow.value(total) == 7
  assert history_values(history) == [3, 7]
  dataflow.unsubscribe(subscription)
}

fn map2_tracks_each_dependency() {
  let context = dataflow.dataflow()
  let first = dataflow.state(0)
  let second = dataflow.state(0)
  let total =
    dataflow.map2(
      fn(first, second) { first + second },
      dataflow.mutation(first),
      dataflow.mutation(second),
    )
  let history = new_history()
  let subscription =
    dataflow.effect(fn(value) { push_history(history, value) }, total)

  let assert Ok(Nil) =
    dataflow.txn(context, fn() { dataflow.set(context, first, 1) })
  let assert Ok(Nil) =
    dataflow.txn(context, fn() { dataflow.set(context, first, 2) })
  let assert Ok(Nil) =
    dataflow.txn(context, fn() { dataflow.set(context, first, 3) })
  let assert Ok(Nil) =
    dataflow.txn(context, fn() { dataflow.set(context, second, 1) })

  assert history_values(history) == [0, 1, 2, 3, 4]
  dataflow.unsubscribe(subscription)
}

fn map3_tracks_each_dependency() {
  let context = dataflow.dataflow()
  let first = dataflow.state(0)
  let second = dataflow.state(0)
  let third = dataflow.state(0)
  let total =
    dataflow.map3(
      fn(first, second, third) { first + second + third },
      dataflow.mutation(first),
      dataflow.mutation(second),
      dataflow.mutation(third),
    )
  let history = new_history()
  let subscription =
    dataflow.effect(fn(value) { push_history(history, value) }, total)

  let assert Ok(Nil) =
    dataflow.txn(context, fn() { dataflow.set(context, first, 1) })
  let assert Ok(Nil) =
    dataflow.txn(context, fn() { dataflow.set(context, first, 2) })
  let assert Ok(Nil) =
    dataflow.txn(context, fn() { dataflow.set(context, third, 1) })

  assert history_values(history) == [0, 1, 2, 3]
  dataflow.unsubscribe(subscription)
}

fn effect_arities() {
  let context = dataflow.dataflow()
  let first = dataflow.state("a")
  let second = dataflow.state("b")
  let third = dataflow.state("c")
  let history = new_history()
  let subscription =
    dataflow.effect2(
      fn(first, second) { push_history(history, #("two", first, second)) },
      dataflow.to_computation(dataflow.mutation(first)),
      dataflow.to_computation(dataflow.mutation(second)),
    )
  assert history_values(history) == [#("two", "a", "b")]
  let assert Ok(Nil) =
    dataflow.txn(context, fn() { dataflow.set(context, first, "x") })
  assert history_values(history) == [#("two", "a", "b"), #("two", "x", "b")]
  dataflow.unsubscribe(subscription)

  let three_history = new_history()
  let after_initial =
    dataflow.effect3_after_initial(
      fn(first, second, third) {
        push_history(three_history, #(first, second, third))
      },
      dataflow.to_computation(dataflow.mutation(first)),
      dataflow.to_computation(dataflow.mutation(second)),
      dataflow.to_computation(dataflow.mutation(third)),
    )
  assert history_values(history) == [#("two", "a", "b"), #("two", "x", "b")]
  let assert Ok(Nil) =
    dataflow.txn(context, fn() { dataflow.set(context, third, "z") })
  assert history_values(history) == [#("two", "a", "b"), #("two", "x", "b")]
  assert history_values(three_history) == [#("x", "b", "z")]
  dataflow.unsubscribe(after_initial)
}

fn partial_graph_effect() {
  let context = dataflow.dataflow()
  let first = dataflow.state("x")
  let first_history = new_history()
  let first_child =
    dataflow.map(
      fn(value) {
        push_history(first_history, value)
        value <> "/" <> value
      },
      dataflow.mutation(first),
    )
  let second = dataflow.state("y")
  let second_history = new_history()
  let second_child =
    dataflow.map(
      fn(value) {
        push_history(second_history, value)
        value <> "?" <> value
      },
      dataflow.mutation(second),
    )
  let effect_history = new_history()
  let subscription =
    dataflow.effect2(
      fn(first, second) { push_history(effect_history, #(first, second)) },
      first_child,
      second_child,
    )
  assert history_values(first_history) == ["x"]
  assert history_values(second_history) == ["y"]
  assert history_values(effect_history) == [#("x/x", "y?y")]
  let assert Ok(Nil) =
    dataflow.txn(context, fn() { dataflow.set(context, first, "a") })
  assert history_values(first_history) == ["x", "a"]
  assert history_values(second_history) == ["y"]
  assert history_values(effect_history) == [#("x/x", "y?y"), #("a/a", "y?y")]
  let assert Ok(Nil) =
    dataflow.txn(context, fn() {
      let assert Ok(Nil) = dataflow.set(context, first, "b")
      dataflow.set(context, second, "z")
    })
  assert history_values(first_history) == ["x", "a", "b"]
  assert history_values(second_history) == ["y", "z"]
  assert history_values(effect_history)
    == [#("x/x", "y?y"), #("a/a", "y?y"), #("b/b", "z?z")]
  dataflow.unsubscribe(subscription)
}

fn handler2_values() {
  let context = dataflow.dataflow()
  let first = dataflow.state("x")
  let second = dataflow.state("y")
  let callback =
    dataflow.handler2(
      fn(event, first, second) { event <> "," <> first <> "," <> second },
      dataflow.mutation(first),
      dataflow.mutation(second),
    )
  assert callback("1") == "1,x,y"
  let assert Ok(Nil) =
    dataflow.txn(context, fn() {
      let assert Ok(Nil) = dataflow.set(context, first, "a")
      dataflow.set(context, second, "b")
    })
  assert callback("2") == "2,a,b"
}

fn map3_values() {
  let total =
    dataflow.map3(
      fn(first, second, third) { first + second + third },
      dataflow.literal(1),
      dataflow.literal(2),
      dataflow.literal(3),
    )
  assert dataflow.value(total) == 6
}

fn derived2_state() {
  let context = dataflow.dataflow()
  let first_state = dataflow.state(1)
  let second_state = dataflow.state(2)
  let total =
    dataflow.derived2(
      fn(first, second) { first + second },
      fn(context, new_total, _first_value, second_value) {
        let assert Ok(Nil) =
          dataflow.set(context, first_state, new_total - second_value)
        dataflow.set(context, second_state, second_value)
      },
      dataflow.mutation(first_state),
      dataflow.mutation(second_state),
    )
  let assert Ok(Nil) =
    dataflow.txn(context, fn() { dataflow.set(context, total, 8) })
  assert dataflow.value(dataflow.to_computation(dataflow.mutation(first_state)))
    == 6
  assert dataflow.value(
      dataflow.to_computation(dataflow.mutation(second_state)),
    )
    == 2
  assert dataflow.value(dataflow.to_computation(dataflow.mutation(total))) == 8
}

fn explicit_field_mapping() {
  let parent = dataflow.pure(Point(3, 4))
  let x = dataflow.field(dataflow.reactive(parent), fn(point) { point.x })
  assert dataflow.value(x) == 3

  // Mirrors the source's `parent[0]` tuple shortcut with the same explicit
  // projection API. The getter is still checked against the concrete list
  // element type at the call site.
  let tuple_parent = dataflow.pure([7])
  let first =
    dataflow.field(dataflow.reactive(tuple_parent), fn(values) {
      let assert [first, ..] = values
      first
    })
  assert dataflow.value(first) == 7

  let context = dataflow.dataflow()
  let mutable_parent = dataflow.state(Point(1, 2))
  let mutable_x =
    dataflow.field_mutation(
      mutable_parent,
      fn(point) { point.x },
      fn(context, value, point) {
        dataflow.set(context, mutable_parent, Point(value, point.y))
      },
    )
  let assert Ok(Nil) =
    dataflow.txn(context, fn() { dataflow.set(context, mutable_x, 9) })
  assert dataflow.value(
      dataflow.to_computation(dataflow.mutation(mutable_parent)),
    )
    == Point(9, 2)
  assert dataflow.value(dataflow.to_computation(dataflow.mutation(mutable_x)))
    == 9

  let mutable_tuple = dataflow.state(["a", "b"])
  let mutable_first =
    dataflow.field_mutation(
      mutable_tuple,
      fn(values) {
        let assert [first, ..] = values
        first
      },
      fn(context, value, values) {
        let assert [_, ..rest] = values
        dataflow.set(context, mutable_tuple, [value, ..rest])
      },
    )
  let assert Ok(Nil) =
    dataflow.txn(context, fn() { dataflow.set(context, mutable_first, "z") })
  assert dataflow.value(
      dataflow.to_computation(dataflow.mutation(mutable_tuple)),
    )
    == ["z", "b"]
  assert dataflow.value(
      dataflow.to_computation(dataflow.mutation(mutable_first)),
    )
    == "z"
}

fn async_computations(delay_counter: History(Nil)) {
  let success =
    dataflow.async_map(dataflow.async_pure(2), fn(value) {
      delayed(delay_counter, value + 1)
    })
  dataflow.async_value(success, fn(result) {
    assert result == Ok(3)
    io.println("lib-dataflow async callback parity passed")
  })

  let failure =
    dataflow.async_map(dataflow.async_pure(2), fn(_value) {
      failed("async failure")
    })
  dataflow.async_value(failure, fn(result) {
    assert result == Error("async failure")
    io.println("lib-dataflow async error parity passed")
  })

  let failure_history = new_history()
  let cached_failure =
    dataflow.async_map(dataflow.async_pure(2), fn(value) {
      push_history(failure_history, value)
      failed("cached async failure")
    })
  dataflow.async_value(cached_failure, fn(result) {
    assert result == Error("cached async failure")
    dataflow.async_value(cached_failure, fn(result) {
      assert result == Error("cached async failure")
      assert history_values(failure_history) == [2]
    })
  })
}

fn async_multiple_values(delay_counter: History(Nil)) {
  clear_history(delay_counter)
  let first = delayed_computation(delay_counter, 2)
  let second = delayed_computation(delay_counter, 3)
  let total =
    dataflow.async_map2(first, second, fn(first, second) {
      fn(done) { done(Ok(first + second)) }
    })
  dataflow.async_value(total, fn(result) {
    assert result == Ok(5)
    io.println("lib-dataflow async multi-value parity passed")
  })
  assert list.length(history_values(delay_counter)) == 2

  let triple =
    dataflow.async_map3(
      dataflow.async_pure(1),
      dataflow.async_pure(2),
      dataflow.async_pure(3),
      fn(first, second, third) { fn(done) { done(Ok(first + second + third)) } },
    )
  let handler =
    dataflow.handler_async2(
      fn(event, left, right) {
        fn(done) { done(Ok(event <> ":" <> left <> ":" <> right)) }
      },
      dataflow.literal("left"),
      dataflow.literal("right"),
    )
  handler("event")(fn(result) {
    assert result == Ok("event:left:right")
  })
  dataflow.async_value(triple, fn(result) {
    assert result == Ok(6)
  })
}

fn async_map_epoch_cache(delay_counter: History(Nil)) {
  clear_history(delay_counter)
  let transform_history = new_history()
  let first_history = new_history()
  let second_history = new_history()
  let computation =
    dataflow.async_map(dataflow.async_pure("source"), fn(value) {
      push_history(transform_history, value)
      delayed_string(delay_counter, value <> "-mapped")
    })

  dataflow.async_value(computation, fn(result) {
    push_history(first_history, result)
  })
  dataflow.async_value(computation, fn(result) {
    push_history(second_history, result)
  })

  // The source MapImpl installs its in-flight update before starting the
  // dependency, so concurrent reads share one transform and one async tail.
  assert history_values(transform_history) == ["source"]
  assert list.length(history_values(delay_counter)) == 1

  delay(delay_counter, 0, fn() {
    assert history_values(first_history) == [Ok("source-mapped")]
    assert history_values(second_history) == [Ok("source-mapped")]
    dataflow.async_value(computation, fn(result) {
      assert result == Ok("source-mapped")
      assert history_values(transform_history) == ["source"]
      io.println("lib-dataflow async epoch cache parity passed")
    })
  })
}

fn async_map_multi_epoch_cache(delay_counter: History(Nil)) {
  let transform_history = new_history()
  let first_history = new_history()
  let second_history = new_history()
  let computation =
    dataflow.async_map2(
      dataflow.async_pure(1),
      dataflow.async_pure(2),
      fn(first, second) {
        push_history(transform_history, #(first, second))
        delayed_string(delay_counter, "three")
      },
    )
  let finish = fn() {
    assert history_values(transform_history) == [#(1, 2)]
    assert history_values(first_history) == [Ok("three")]
    assert history_values(second_history) == [Ok("three")]
    dataflow.async_value(computation, fn(result) {
      assert result == Ok("three")
      assert history_values(transform_history) == [#(1, 2)]
      io.println("lib-dataflow async multi-input cache parity passed")
    })
  }

  dataflow.async_value(computation, fn(result) {
    push_history(first_history, result)
    case list.length(history_values(second_history)) {
      1 -> finish()
      _ -> Nil
    }
  })
  dataflow.async_value(computation, fn(result) {
    push_history(second_history, result)
    case list.length(history_values(first_history)) {
      1 -> finish()
      _ -> Nil
    }
  })
}

fn async_state_and_effect() {
  let context = dataflow.async_context()
  let first = dataflow.async_state(0)
  let second = dataflow.async_state(0)
  let history = new_history()

  let first_effect =
    dataflow.async_effect(dataflow.async_mutation(first), fn(value) {
      push_history(history, #("first", value))
      immediate_unit()
    })
  first_effect(fn(result) {
    let assert Ok(first_subscription) = result
    let second_effect =
      dataflow.async_effect(dataflow.async_mutation(second), fn(value) {
        push_history(history, #("second", value))
        immediate_unit()
      })
    second_effect(fn(result) {
      let assert Ok(second_subscription) = result
      let transaction =
        dataflow.async_txn(context, fn() {
          let assert Ok(Nil) = dataflow.async_set(context, first, 1)
          let assert Ok(Nil) = dataflow.async_set(context, second, 2)
          immediate_value(Nil)
        })
      transaction(fn(result) {
        assert result == Ok(Nil)
        assert history_values(history)
          == [#("first", 0), #("second", 0), #("first", 1), #("second", 2)]
        dataflow.async_unsubscribe(first_subscription)
        dataflow.async_unsubscribe(second_subscription)
        io.println("lib-dataflow async state/effect parity passed")
      })
    })
  })
}

fn async_errors_propagate_through_transactions() {
  let context = dataflow.async_context()
  let state = dataflow.async_state("a")
  let effect =
    dataflow.async_effect(dataflow.async_mutation(state), fn(value) {
      case value {
        "a" -> immediate_unit()
        _ -> failed_unit("effect failure")
      }
    })
  effect(fn(result) {
    let assert Ok(subscription) = result
    let transaction =
      dataflow.async_txn(context, fn() {
        let assert Ok(Nil) = dataflow.async_set(context, state, "b")
        immediate_value(Nil)
      })
    transaction(fn(result) {
      assert result == Error("effect failure")
      dataflow.async_value(dataflow.async_mutation(state), fn(result) {
        assert result == Ok("b")
        dataflow.async_unsubscribe(subscription)

        let mapped =
          dataflow.async_map(dataflow.async_mutation(state), fn(value) {
            case value {
              "c" -> failed_string("map failure")
              _ -> immediate_value(value)
            }
          })
        let mapped_effect =
          dataflow.async_effect(mapped, fn(_value) { immediate_unit() })
        mapped_effect(fn(result) {
          let assert Ok(mapped_subscription) = result
          let next_transaction =
            dataflow.async_txn(context, fn() {
              let assert Ok(Nil) = dataflow.async_set(context, state, "c")
              immediate_value(Nil)
            })
          next_transaction(fn(result) {
            assert result == Error("map failure")
            dataflow.async_value(mapped, fn(result) {
              assert result == Error("map failure")
              dataflow.async_unsubscribe(mapped_subscription)
              io.println("lib-dataflow async rejection propagation passed")
            })
          })
        })
      })
    })
  })
}

fn async_derived_and_effect_multiple() {
  let context = dataflow.async_context()
  let first = dataflow.async_state("x")
  let second = dataflow.async_state("y")
  let derived =
    dataflow.async_derived2(
      dataflow.async_mutation(first),
      dataflow.async_mutation(second),
      fn(first, second) { immediate_value(#(first, second)) },
      fn(context, value, old_first, old_second) {
        let #(new_first, new_second) = value
        assert old_first == "x"
        assert old_second == "y"
        let assert Ok(Nil) = dataflow.async_set(context, first, new_first)
        let assert Ok(Nil) = dataflow.async_set(context, second, new_second)
        immediate_unit()
      },
    )
  let set_derived =
    dataflow.async_txn(context, fn() {
      dataflow.async_set_derived2(context, derived, #("a", "b"))
    })
  set_derived(fn(result) {
    assert result == Ok(Nil)
    dataflow.async_value(
      dataflow.async_derived2_computation(derived),
      fn(result) {
        assert result == Ok(#("a", "b"))
        let history = new_history()
        let effect =
          dataflow.async_effect2(
            dataflow.async_mutation(first),
            dataflow.async_mutation(second),
            fn(first, second) {
              push_history(history, #(first, second))
              immediate_unit()
            },
          )
        effect(fn(result) {
          let assert Ok(subscription) = result
          assert history_values(history) == [#("a", "b")]
          dataflow.async_unsubscribe(subscription)
          let triple_history = new_history()
          let triple_effect =
            dataflow.async_effect3(
              dataflow.async_pure(1),
              dataflow.async_pure(2),
              dataflow.async_pure(3),
              fn(first, second, third) {
                push_history(triple_history, #(first, second, third))
                immediate_unit()
              },
            )
          triple_effect(fn(result) {
            let assert Ok(triple_subscription) = result
            assert history_values(triple_history) == [#(1, 2, 3)]
            dataflow.async_unsubscribe(triple_subscription)
            io.println("lib-dataflow async derived/effect arity parity passed")
          })
        })
      },
    )
  })
}

fn async_derived_in_progress_epoch(
  delay_counter: History(Nil),
  done: fn() -> Nil,
) {
  clear_history(delay_counter)
  let context = dataflow.async_context()
  let state = dataflow.async_state("a")
  let setter_history = new_history()
  let mapped =
    dataflow.async_map(dataflow.async_mutation(state), fn(value) {
      delayed_string(delay_counter, value <> "-mapped")
    })
  let derived =
    dataflow.async_derived1(
      mapped,
      fn(value) { immediate_value(value) },
      fn(_context, _value, dependency) {
        push_history(setter_history, dependency)
        immediate_unit()
      },
    )
  let derived_effect =
    dataflow.async_effect(
      dataflow.async_derived1_computation(derived),
      fn(_value) { immediate_unit() },
    )
  derived_effect(fn(result) {
    let assert Ok(derived_subscription) = result
    let source_effect =
      dataflow.async_effect(dataflow.async_mutation(state), fn(value) {
        case value {
          "a" -> immediate_unit()
          "b" ->
            dataflow.async_txn(context, fn() {
              dataflow.async_set_derived1(context, derived, "next")
            })
          _ -> immediate_unit()
        }
      })
    source_effect(fn(result) {
      let assert Ok(source_subscription) = result
      let transaction =
        dataflow.async_txn(context, fn() {
          let assert Ok(Nil) = dataflow.async_set(context, state, "b")
          immediate_value(Nil)
        })
      transaction(fn(result) {
        assert result == Ok(Nil)
        assert history_values(setter_history) == ["b-mapped"]
        dataflow.async_unsubscribe(source_subscription)
        dataflow.async_unsubscribe(derived_subscription)
        io.println("lib-dataflow async derived in-progress epoch parity passed")
        done()
      })
    })
  })
}

fn async_derived2_in_progress_epoch(
  delay_counter: History(Nil),
  done: fn() -> Nil,
) {
  clear_history(delay_counter)
  let context = dataflow.async_context()
  let first_state = dataflow.async_state("a")
  let setter_history = new_history()
  let first =
    dataflow.async_map(dataflow.async_mutation(first_state), fn(value) {
      delayed_string(delay_counter, value <> "-mapped")
    })
  let second = dataflow.async_pure("x")
  let derived =
    dataflow.async_derived2(
      first,
      second,
      fn(first, second) { immediate_value(first <> second) },
      fn(_context, _value, first, second) {
        push_history(setter_history, #(first, second))
        immediate_unit()
      },
    )
  let derived_effect =
    dataflow.async_effect(
      dataflow.async_derived2_computation(derived),
      fn(_value) { immediate_unit() },
    )
  derived_effect(fn(result) {
    let assert Ok(derived_subscription) = result
    let source_effect =
      dataflow.async_effect(dataflow.async_mutation(first_state), fn(value) {
        case value {
          "a" -> immediate_unit()
          "b" ->
            dataflow.async_txn(context, fn() {
              dataflow.async_set_derived2(context, derived, "next")
            })
          _ -> immediate_unit()
        }
      })
    source_effect(fn(result) {
      let assert Ok(source_subscription) = result
      let transaction =
        dataflow.async_txn(context, fn() {
          let assert Ok(Nil) = dataflow.async_set(context, first_state, "b")
          immediate_value(Nil)
        })
      transaction(fn(result) {
        assert result == Ok(Nil)
        assert history_values(setter_history) == [#("b-mapped", "x")]
        dataflow.async_unsubscribe(source_subscription)
        dataflow.async_unsubscribe(derived_subscription)
        io.println(
          "lib-dataflow async derived2 in-progress epoch parity passed",
        )
        done()
      })
    })
  })
}

// Source: lib-dataflow/index.ts DataflowImpl.commitTransaction and
// MapImpl.update. An effect may commit another state while the current epoch
// is still notifying subscribers. The source reuses that epoch, so the
// second state value is committed but the already-running map iteration is
// not replaced by a new one.
fn async_nested_transaction_epoch(
  delay_counter: History(Nil),
  done: fn() -> Nil,
) {
  clear_history(delay_counter)
  let context = dataflow.async_context()
  let state = dataflow.async_state("a")
  let transform_history = new_history()
  let effect_history = new_history()
  let state_effect_history = new_history()
  let mapped =
    dataflow.async_map(dataflow.async_mutation(state), fn(value) {
      push_history(transform_history, value)
      delayed_string(delay_counter, value <> "-mapped")
    })
  let effect =
    dataflow.async_effect(mapped, fn(value) {
      push_history(effect_history, value)
      case value {
        "b-mapped" ->
          dataflow.async_txn(context, fn() {
            let assert Ok(Nil) = dataflow.async_set(context, state, "c")
            immediate_value(Nil)
          })
        _ -> immediate_unit()
      }
    })
  effect(fn(result) {
    let assert Ok(subscription) = result
    let state_effect =
      dataflow.async_effect(dataflow.async_mutation(state), fn(value) {
        delayed_history(delay_counter, state_effect_history, value)
      })
    state_effect(fn(result) {
      let assert Ok(state_subscription) = result
      let transaction =
        dataflow.async_txn(context, fn() {
          let assert Ok(Nil) = dataflow.async_set(context, state, "b")
          immediate_value(Nil)
        })
      transaction(fn(result) {
        assert result == Ok(Nil)
        assert history_values(transform_history) == ["a", "b"]
        assert history_values(effect_history) == ["a-mapped", "b-mapped"]
        // DataflowImpl.commitTransaction does not wait for an epoch that it
        // did not create. The nested set to c is committed synchronously,
        // while its subscriber callback is still pending.
        assert history_values(state_effect_history) == ["a", "b"]
        assert dataflow.async_value(dataflow.async_mutation(state), fn(result) {
            assert result == Ok("c")
          })
          == Nil
        dataflow.async_value(mapped, fn(result) {
          assert result == Ok("b-mapped")
          delay(delay_counter, 0, fn() {
            assert history_values(state_effect_history) == ["a", "b", "c"]
            dataflow.async_unsubscribe(subscription)
            dataflow.async_unsubscribe(state_subscription)
            io.println("lib-dataflow async nested epoch parity passed")
            done()
          })
        })
      })
    })
  })
}

fn async_map_state_epoch(delay_counter: History(Nil)) {
  clear_history(delay_counter)
  let context = dataflow.async_context()
  let state = dataflow.async_state("a")
  let transform_history = new_history()
  let effect_history = new_history()
  let mapped =
    dataflow.async_map(dataflow.async_mutation(state), fn(value) {
      push_history(transform_history, value)
      delayed_string(delay_counter, value <> "-mapped")
    })
  let effect =
    dataflow.async_effect(mapped, fn(value) {
      push_history(effect_history, value)
      immediate_unit()
    })
  effect(fn(result) {
    let assert Ok(subscription) = result
    assert history_values(transform_history) == ["a"]
    assert history_values(effect_history) == ["a-mapped"]
    let transaction =
      dataflow.async_txn(context, fn() {
        let assert Ok(Nil) = dataflow.async_set(context, state, "b")
        immediate_value(Nil)
      })
    transaction(fn(result) {
      assert result == Ok(Nil)
      assert history_values(transform_history) == ["a", "b"]
      assert history_values(effect_history) == ["a-mapped", "b-mapped"]
      dataflow.async_unsubscribe(subscription)
      io.println("lib-dataflow async map state epoch parity passed")
    })
  })
}

fn async_map_error_persists() {
  let context = dataflow.async_context()
  let state = dataflow.async_state("a")
  let transform_history = new_history()
  let mapped =
    dataflow.async_map(dataflow.async_mutation(state), fn(value) {
      push_history(transform_history, value)
      case value {
        "bad" -> failed_string("permanent map failure")
        _ -> immediate_value(value)
      }
    })
  let effect = dataflow.async_effect(mapped, fn(_value) { immediate_unit() })
  effect(fn(result) {
    let assert Ok(subscription) = result
    let bad_transaction =
      dataflow.async_txn(context, fn() {
        let assert Ok(Nil) = dataflow.async_set(context, state, "bad")
        immediate_value(Nil)
      })
    bad_transaction(fn(result) {
      assert result == Error("permanent map failure")
      let recovery_transaction =
        dataflow.async_txn(context, fn() {
          let assert Ok(Nil) = dataflow.async_set(context, state, "good")
          immediate_value(Nil)
        })
      recovery_transaction(fn(result) {
        assert result == Error("permanent map failure")
        dataflow.async_value(mapped, fn(result) {
          assert result == Error("permanent map failure")
          assert history_values(transform_history) == ["a", "bad"]
          dataflow.async_unsubscribe(subscription)
          io.println("lib-dataflow async map rejection persistence passed")
        })
      })
    })
  })
}

fn async_effect_error_persists() {
  let context = dataflow.async_context()
  let state = dataflow.async_state("a")
  let effect_history = new_history()
  let effect =
    dataflow.async_effect(dataflow.async_mutation(state), fn(value) {
      push_history(effect_history, value)
      case value {
        "bad" -> failed_unit("permanent effect failure")
        _ -> immediate_unit()
      }
    })
  effect(fn(result) {
    let assert Ok(subscription) = result
    let bad_transaction =
      dataflow.async_txn(context, fn() {
        let assert Ok(Nil) = dataflow.async_set(context, state, "bad")
        immediate_value(Nil)
      })
    bad_transaction(fn(result) {
      assert result == Error("permanent effect failure")
      let recovery_transaction =
        dataflow.async_txn(context, fn() {
          let assert Ok(Nil) = dataflow.async_set(context, state, "good")
          immediate_value(Nil)
        })
      recovery_transaction(fn(result) {
        assert result == Error("permanent effect failure")
        assert history_values(effect_history) == ["a", "bad"]
        dataflow.async_unsubscribe(subscription)
        io.println("lib-dataflow async effect rejection persistence passed")
      })
    })
  })
}

fn async_map_queued_epochs(delay_counter: History(Nil), done: fn() -> Nil) {
  clear_history(delay_counter)
  let context = dataflow.async_context()
  let state = dataflow.async_state("a")
  let transform_history = new_history()
  let effect_history = new_history()
  let mapped =
    dataflow.async_map(dataflow.async_mutation(state), fn(value) {
      push_history(transform_history, value)
      delayed_string(delay_counter, value <> "-mapped")
    })
  let effect =
    dataflow.async_effect(mapped, fn(value) {
      push_history(effect_history, value)
      immediate_unit()
    })
  effect(fn(result) {
    let assert Ok(subscription) = result
    let first_transaction =
      dataflow.async_txn(context, fn() {
        let assert Ok(Nil) = dataflow.async_set(context, state, "b")
        immediate_value(Nil)
      })
    first_transaction(fn(result) {
      assert result == Ok(Nil)
    })

    // Commit another state value while the first mapped update is still
    // waiting. The source reuses the in-progress epoch, so MapImpl serves
    // the already-running iteration instead of replacing it with a second
    // transform. The state itself still contains the latest committed value.
    let second_transaction =
      dataflow.async_txn(context, fn() {
        let assert Ok(Nil) = dataflow.async_set(context, state, "c")
        immediate_value(Nil)
      })
    second_transaction(fn(result) {
      assert result == Ok(Nil)
      // A nested commit returns before the active epoch's subscribers have
      // drained. Observe the computation before asserting notification
      // history, just as the source epoch wait does for its owner.
      dataflow.async_value(mapped, fn(result) {
        assert result == Ok("b-mapped")
        assert history_values(transform_history) == ["a", "b"]
        assert history_values(effect_history) == ["a-mapped", "b-mapped"]
        dataflow.async_value(dataflow.async_mutation(state), fn(result) {
          assert result == Ok("c")
        })
        dataflow.async_unsubscribe(subscription)
        io.println("lib-dataflow async map queued epochs parity passed")
        done()
      })
    })
  })
}

fn delayed_computation(
  delay_counter: History(Nil),
  value: Int,
) -> dataflow.AsyncComputation(Int, String) {
  dataflow.async_pure(value)
  |> dataflow.async_map(fn(value) { delayed(delay_counter, value) })
}

fn delayed(
  delay_counter: History(Nil),
  value: Int,
) -> dataflow.Async(Int, String) {
  fn(done) { delay(delay_counter, 0, fn() { done(Ok(value)) }) }
}

fn delayed_string(
  delay_counter: History(Nil),
  value: String,
) -> dataflow.Async(String, String) {
  fn(done) { delay(delay_counter, 0, fn() { done(Ok(value)) }) }
}

fn failed(reason: String) -> dataflow.Async(Int, String) {
  fn(done) { done(Error(reason)) }
}

fn failed_unit(reason: String) -> dataflow.Async(Nil, String) {
  fn(done) { done(Error(reason)) }
}

fn failed_string(reason: String) -> dataflow.Async(String, String) {
  fn(done) { done(Error(reason)) }
}

fn immediate_unit() -> dataflow.Async(Nil, String) {
  fn(done) { done(Ok(Nil)) }
}

fn delayed_history(
  delay_counter: History(Nil),
  history: History(value),
  value: value,
) -> dataflow.Async(Nil, String) {
  fn(done) {
    delay(delay_counter, 0, fn() {
      push_history(history, value)
      done(Ok(Nil))
    })
  }
}

fn immediate_value(value: value) -> dataflow.Async(value, String) {
  fn(done) { done(Ok(value)) }
}

fn history_values(history: History(value)) -> List(value) {
  let History(_, state) = history
  dataflow.value(dataflow.to_computation(dataflow.mutation(state)))
  |> list.reverse
}
