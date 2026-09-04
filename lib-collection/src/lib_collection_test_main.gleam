import gleam/dict
import gleam/io
import gleam/list
import gleam/option.{None, Some}
import lib_collection as collection

type ObjectKey

type NumberKey

@external(javascript, "./lib_collection_test_identity_ffi.mjs", "new_object")
fn new_object() -> ObjectKey

@external(javascript, "./lib_collection_test_identity_ffi.mjs", "new_nan")
fn new_nan() -> NumberKey

@external(javascript, "./lib_collection_test_identity_ffi.mjs", "new_negative_zero")
fn new_negative_zero() -> NumberKey

@external(javascript, "./lib_collection_test_identity_ffi.mjs", "new_positive_zero")
fn new_positive_zero() -> NumberKey

@external(javascript, "./lib_collection_test_identity_ffi.mjs", "call_and_catch")
fn call_and_catch(callback: fn() -> String) -> String

pub fn run(done: fn() -> Nil) {
  test_group_by()
  test_memo()
  test_identity_keys()
  test_multi_memo()
  test_multi_identity_keys()
  test_object_identity_keys()
  test_same_value_zero_keys()
  test_multi_memo_variable_arity()
  test_template()
  test_strip_prefix()
  test_stateful_regex()
  io.println("lib-collection Gleam parity tests passed")
  done()
}

pub fn main() {
  run(fn() { Nil })
}

fn test_group_by() {
  let assert [] = collection.group_by([], fn(n) { n })
  let assert [#(1, [1, 1, 5, 9]), #(0, [2, 8])] =
    collection.group_by([1, 1, 2, 5, 8, 9], fn(n) { n % 2 })
}

fn test_memo() {
  let memo = collection.memo(fn(n: Int) { n * 2 })
  let #(memo, first) = collection.get(memo, 1)
  let #(memo, second) = collection.get(memo, 1)
  let #(memo, third) = collection.get(memo, 2)
  assert first == 2
  assert second == 2
  assert third == 4
  assert collection.entries(memo) == [#(1, 2), #(2, 4)]
  assert collection.keys(memo) == [1, 2]
  assert collection.values(memo) == [2, 4]
  let memo = collection.delete(memo, 1)
  let #(memo, after_delete) = collection.get(memo, 1)
  assert after_delete == 2
  assert collection.entries(memo) == [#(2, 4), #(1, 2)]
}

fn test_identity_keys() {
  let first_key = fn(value: Int) { value }
  let second_key = fn(value: Int) { value }
  let memo = collection.memo(fn(_key: fn(Int) -> Int) { "value" })
  let #(memo, _) = collection.get(memo, first_key)
  let #(memo, _) = collection.get(memo, first_key)
  let #(memo, _) = collection.get(memo, second_key)
  assert list.length(collection.entries(memo)) == 2
}

fn test_multi_memo() {
  let memo = collection.multi_memo(fn(key: List(Int)) { sum(key) })
  let #(memo, first) = collection.multi_get(memo, [1, 2, 3])
  let #(memo, second) = collection.multi_get(memo, [1, 2, 3])
  let #(memo, empty) = collection.multi_get(memo, [])
  let #(memo, one) = collection.multi_get(memo, [1])
  assert first == 6
  assert second == 6
  assert empty == 0
  assert one == 1
  let #(_, two_two_three) = collection.multi_get(memo, [2, 2, 3])
  assert two_two_three == 7
}

fn test_multi_memo_variable_arity() {
  let memo = collection.multi_memo(fn(key: List(Int)) { sum(key) })
  let #(memo, empty) = collection.multi_get(memo, [])
  let #(memo, one) = collection.multi_get(memo, [1])
  let #(memo, three) = collection.multi_get(memo, [1, 2, 3])
  let #(memo, repeated) = collection.multi_get(memo, [1, 2, 3])
  assert empty == 0
  assert one == 1
  assert three == 6
  assert repeated == 6
  let #(_, two) = collection.multi_get(memo, [2])
  assert two == 2
}

fn test_multi_identity_keys() {
  let first_key = fn(value: Int) { value + 1 }
  let second_key = fn(value: Int) { value + 2 }
  let memo =
    collection.multi_memo(fn(keys: List(fn(Int) -> Int)) {
      case keys {
        [key] -> key(0)
        _ -> 0
      }
    })
  let #(memo, first) = collection.multi_get(memo, [first_key])
  let #(memo, second) = collection.multi_get(memo, [second_key])
  let #(_, repeated) = collection.multi_get(memo, [first_key])
  assert first == 1
  assert second == 2
  assert repeated == 1
}

fn test_object_identity_keys() {
  let first_key = new_object()
  let second_key = new_object()
  let memo = collection.memo(fn(_key: ObjectKey) { "value" })
  let #(memo, first) = collection.get(memo, first_key)
  let #(memo, repeated) = collection.get(memo, first_key)
  let #(memo, second) = collection.get(memo, second_key)
  assert first == "value"
  assert repeated == "value"
  assert second == "value"
  assert list.length(collection.entries(memo)) == 2
}

// JavaScript Map uses SameValueZero: NaN compares equal to NaN and signed
// zero compares equal to signed zero. The memo algorithm remains the Gleam
// ordered-entry model; only this foreign predicate observes JS key semantics.
fn test_same_value_zero_keys() {
  let nan = new_nan()
  let memo = collection.memo(fn(_key: NumberKey) { "nan" })
  let #(memo, first) = collection.get(memo, nan)
  let #(memo, repeated) = collection.get(memo, new_nan())
  assert first == "nan"
  assert repeated == "nan"
  assert list.length(collection.entries(memo)) == 1

  let negative_zero = new_negative_zero()
  let positive_zero = new_positive_zero()
  let memo = collection.memo(fn(_key: NumberKey) { "zero" })
  let #(memo, first) = collection.get(memo, negative_zero)
  let #(memo, repeated) = collection.get(memo, positive_zero)
  assert first == "zero"
  assert repeated == "zero"
  assert list.length(collection.entries(memo)) == 1
}

fn test_template() {
  let pattern = collection.parameter_pattern()
  let empty = dict.new()
  assert collection.apply_template(pattern, "", empty) == ""
  assert collection.apply_template(pattern, "abc", empty) == "abc"
  assert collection.apply_template(
      pattern,
      "aaa${b}ccc",
      dict.from_list([#("b", "q")]),
    )
    == "aaaqccc"
  assert collection.apply_template(
      pattern,
      "0${a} 1${b} 2${a}",
      dict.from_list([#("a", "A"), #("b", "B")]),
    )
    == "0A 1B 2A"
  assert call_and_catch(fn() {
      collection.apply_template(
        pattern,
        "0${a} 1${b} 2${c}",
        dict.from_list([#("a", "A"), #("b", "B")]),
      )
    })
    == "Parameter c not defined"
  assert collection.apply_template(
      collection.css_import_pattern(),
      "@import url('font.css');",
      dict.from_list([#("font", "FONT_CSS")]),
    )
    == "FONT_CSS"
}

fn test_strip_prefix() {
  assert collection.strip_prefix("", collection.repeat("a")) == None
  assert collection.strip_prefix("aaabc", collection.repeat("a")) == Some("bc")
  assert collection.strip_prefix("baaac", collection.repeat("a")) == Some("ac")
  assert collection.strip_prefix(
      "/.code/.id/a",
      collection.prefix("/.code/.id/"),
    )
    == Some("a")
  assert collection.strip_prefix(
      "/prefix/.code/a",
      collection.search("/.code/"),
    )
    == Some("/.code/a")
  assert collection.strip_prefix("/other", collection.search("/.code/")) == None
}

// `stripPrefix` calls RegExp.exec directly, so a caller-supplied global
// pattern retains JavaScript's lastIndex state between calls. This is a real
// runtime property of the source API, not an implementation detail of the
// Gleam list-backed memo model.
fn test_stateful_regex() {
  let pattern = collection.regex("a+", "g")
  assert collection.strip_prefix("aa", pattern) == Some("")
  assert collection.strip_prefix("aa", pattern) == None
  assert collection.strip_prefix("aa", pattern) == Some("")
}

fn sum(items: List(Int)) -> Int {
  case items {
    [] -> 0
    [first, ..rest] -> first + sum(rest)
  }
}
