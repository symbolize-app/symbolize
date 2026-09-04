import gleam/io
import lib_error
import lib_test

@external(javascript, "./lib_test_runtime_ffi.mjs", "set_timeout")
fn set_timeout(callback: fn() -> Nil, milliseconds: Int) -> Nil

@external(javascript, "./lib_test_runtime_ffi.mjs", "call_and_catch")
fn call_and_catch(callback: fn() -> String) -> String

fn delayed_ok(
  value: String,
  milliseconds: Int,
) -> lib_error.Async(String, String) {
  fn(done) { set_timeout(fn() { done(Ok(value)) }, milliseconds) }
}

fn after_delay(milliseconds: Int, callback: fn() -> Nil) -> Nil {
  set_timeout(callback, milliseconds)
}

pub fn main() {
  let assert Ok(Nil) = lib_test.assert_true(True)
  let assert Ok(Nil) = lib_test.assert_equal(2, 2)
  let assert Ok(Nil) = lib_test.assert_deep_equal([1, 2], [1, 2])
  let assert Ok("reason") = lib_test.assert_error(Error("reason"))
  let assert Error(_) = lib_test.assert_error(Ok("value"))
  lib_test.assert_error_async(fn(done) { done(Error("async reason")) })(
    fn(result) {
      assert result == Ok("async reason")
    },
  )
  lib_test.assert_error_async(fn(done) { done(Ok("value")) })(fn(result) {
    case result {
      Error(_) -> Nil
      Ok(_) -> panic as "fulfilled async assertion did not fail"
    }
  })

  let resolved = lib_test.sync(fn(done) { done(Ok("resolved")) })
  let value = lib_test.sync_value("value")
  let rejected = lib_test.sync(fn(done) { done(Error("rejected")) })
  let pending = lib_test.sync(delayed_ok("later", 0))
  let first_settlement =
    lib_test.sync(fn(done) {
      done(Ok("first"))
      done(Error("second"))
    })
  assert lib_test.is_settled(pending) == False
  assert lib_test.is_settled(resolved) == False
  assert lib_test.is_settled(value) == False
  assert lib_test.is_settled(rejected) == False
  assert call_and_catch(fn() { lib_test.resolved_value(resolved) })
    == "Promise not resolved yet"
  assert call_and_catch(fn() { lib_test.resolved_value(value) })
    == "Promise not resolved yet"
  assert call_and_catch(fn() { lib_test.resolved_value(pending) })
    == "Promise not resolved yet"
  after_delay(0, fn() {
    assert lib_test.resolved_value(resolved) == "resolved"
    assert lib_test.resolved_value(value) == "value"
    assert lib_test.resolved_value(first_settlement) == "first"
    assert lib_test.is_settled(resolved) == False
    assert lib_test.is_settled(value) == False
    assert lib_test.is_settled(rejected) == True
    assert lib_test.rejected_value(rejected) == "rejected"
    assert call_and_catch(fn() { lib_test.resolved_value(rejected) })
      == "rejected"
    assert lib_test.resolved_value(pending) == "later"
    // Preserve the source getter typo after the Promise resolves as well.
    assert lib_test.is_settled(pending) == False
    io.println("lib-test SyncPromise parity passed")
  })

  let mock =
    lib_test.mock_with_history([
      fn(value: Int) { value + 1 },
      fn(value: Int) { value + 2 },
    ])
  assert lib_test.mock_history(mock) == []
  let assert Ok(#(2, mock)) = lib_test.mock_call(mock, 1)
  let assert Ok(#(4, mock)) = lib_test.mock_call(mock, 2)
  assert lib_test.mock_history(mock) == [1, 2]
  let assert Error("called too many times") = lib_test.mock_call(mock, 3)
  let repeated =
    lib_test.repeat_mock_with_history(2, fn(value: String) { value <> "!" })
  let assert Ok(#("a!", repeated)) = lib_test.mock_call(repeated, "a")
  let assert Ok(#("b!", repeated)) = lib_test.mock_call(repeated, "b")
  assert lib_test.mock_history(repeated) == ["a", "b"]

  let first =
    lib_test.module("first", [lib_test.new_test("one", fn() { Ok(Nil) })])
  let nested =
    lib_test.collection([
      lib_test.module_item(first),
      lib_test.collection_item(
        lib_test.collection([
          lib_test.module_item(
            lib_test.module("second", [
              lib_test.new_test("two", fn() { Ok(Nil) }),
            ]),
          ),
        ]),
      ),
    ])
  let assert [first_module, second_module] = lib_test.modules(nested)
  let assert Ok(Nil) =
    lib_test.assert_equal(lib_test.module_url(first_module), "first")
  let assert Ok(Nil) =
    lib_test.assert_equal(lib_test.module_url(second_module), "second")
  io.println("lib-test Gleam parity tests passed")
}
