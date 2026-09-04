import gleam/dict
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string

/// A JavaScript `RegExp` supplied through the source API's actual runtime
/// boundary. The matching and replacement algorithms remain in the source
/// functions; this value only represents the foreign regular-expression
/// object and its native operations.
pub type Pattern

@external(javascript, "./pattern_ffi.mjs", "new_pattern")
fn new_pattern(source: String, flags: String) -> Pattern

@external(javascript, "./pattern_ffi.mjs", "exec_substring")
fn exec_substring(pattern: Pattern, input: String) -> #(Bool, String)

@external(javascript, "./pattern_ffi.mjs", "same_key")
fn same_key(first: value, second: value) -> Bool

@external(javascript, "./pattern_ffi.mjs", "replace_template")
fn replace_template(
  pattern: Pattern,
  input: String,
  lookup: fn(String) -> Result(String, String),
) -> Result(String, String)

pub fn regex(source: String, flags: String) -> Pattern {
  new_pattern(source, flags)
}

pub fn prefix(value: String) -> Pattern {
  new_pattern("^" <> regex_escape(value), "")
}

pub fn search(value: String) -> Pattern {
  new_pattern(regex_escape(value), "")
}

pub fn repeat(value: String) -> Pattern {
  new_pattern(regex_escape(value) <> "+", "")
}

pub fn parameter_pattern() -> Pattern {
  new_pattern("\\$\\{(?<parameterName>[^}]+)\\}", "g")
}

pub fn css_import_pattern() -> Pattern {
  new_pattern("@import url\\('(?<parameterName>[^']+)\\.css'\\);", "g")
}

pub opaque type Memo(key, value) {
  Memo(fn(key) -> value, List(#(key, value)))
}

pub fn memo(builder: fn(key) -> value) -> Memo(key, value) {
  Memo(builder, [])
}

pub fn get(memo: Memo(key, value), key: key) -> #(Memo(key, value), value) {
  case memo {
    Memo(builder, values) ->
      case find_value(values, key) {
        Some(value) -> #(memo, value)
        None -> {
          let value = builder(key)
          #(Memo(builder, append(values, #(key, value))), value)
        }
      }
  }
}

pub fn delete(memo: Memo(key, value), key: key) -> Memo(key, value) {
  case memo {
    Memo(builder, values) ->
      case find_value(values, key) {
        Some(_) -> Memo(builder, remove_value(values, key))
        None -> memo
      }
  }
}

pub fn entries(memo: Memo(key, value)) -> List(#(key, value)) {
  case memo {
    Memo(_, values) -> values
  }
}

pub fn keys(memo: Memo(key, value)) -> List(key) {
  entries(memo)
  |> list.map(fn(entry) { entry.0 })
}

pub fn values(memo: Memo(key, value)) -> List(value) {
  entries(memo)
  |> list.map(fn(entry) { entry.1 })
}

pub opaque type MultiMemo(key, value) {
  MultiMemo(fn(List(key)) -> value, List(#(List(key), value)))
}

pub fn multi_memo(builder: fn(List(key)) -> value) -> MultiMemo(key, value) {
  MultiMemo(builder, [])
}

pub fn multi_get(
  memo: MultiMemo(key, value),
  key: List(key),
) -> #(MultiMemo(key, value), value) {
  case memo {
    MultiMemo(builder, values) ->
      case find_multi_value(values, key) {
        Some(value) -> #(memo, value)
        None -> {
          let value = builder(key)
          #(MultiMemo(builder, append(values, #(key, value))), value)
        }
      }
  }
}

pub fn group_by(
  items: List(item),
  selector: fn(item) -> key,
) -> List(#(key, List(item))) {
  let groups = memo(fn(_key: key) { [] })
  let groups = add_group(groups, items, selector)
  entries(groups)
}

fn add_group(
  groups: Memo(key, List(item)),
  items: List(item),
  selector: fn(item) -> key,
) -> Memo(key, List(item)) {
  case items {
    [] -> groups
    [item, ..rest] -> {
      let key = selector(item)
      let #(groups, old_items) = get(groups, key)
      let groups = put_memo(groups, key, append(old_items, item))
      add_group(groups, rest, selector)
    }
  }
}

fn put_memo(
  memo: Memo(key, value),
  key: key,
  value: value,
) -> Memo(key, value) {
  case memo {
    Memo(builder, values) -> Memo(builder, upsert_value(values, key, value))
  }
}

pub fn apply_template(
  pattern: Pattern,
  template: String,
  parameters: dict.Dict(String, String),
) -> String {
  case
    replace_template(pattern, template, fn(name) {
      case dict.get(parameters, name) {
        Ok(value) -> Ok(value)
        Error(_) -> Error("Parameter " <> name <> " not defined")
      }
    })
  {
    Ok(result) -> result
    Error(reason) -> panic as reason
  }
}

pub fn strip_prefix(input: String, pattern: Pattern) -> Option(String) {
  let #(matched, result) = exec_substring(pattern, input)
  case matched {
    True -> Some(result)
    False -> None
  }
}

fn find_value(values: List(#(key, value)), key: key) -> Option(value) {
  case values {
    [] -> None
    [#(entry_key, value), ..rest] ->
      case same_key(entry_key, key) {
        True -> Some(value)
        False -> find_value(rest, key)
      }
  }
}

fn find_multi_value(
  values: List(#(List(key), value)),
  key: List(key),
) -> Option(value) {
  case values {
    [] -> None
    [#(entry_key, value), ..rest] ->
      case same_keys(entry_key, key) {
        True -> Some(value)
        False -> find_multi_value(rest, key)
      }
  }
}

fn same_keys(first: List(key), second: List(key)) -> Bool {
  case first, second {
    [], [] -> True
    [first, ..first_rest], [second, ..second_rest] ->
      same_key(first, second) && same_keys(first_rest, second_rest)
    _, _ -> False
  }
}

fn upsert_value(
  values: List(#(key, value)),
  key: key,
  value: value,
) -> List(#(key, value)) {
  case values {
    [] -> [#(key, value)]
    [#(entry_key, old_value), ..rest] ->
      case same_key(entry_key, key) {
        True -> [#(entry_key, value), ..rest]
        False -> [#(entry_key, old_value), ..upsert_value(rest, key, value)]
      }
  }
}

fn remove_value(values: List(#(key, value)), key: key) -> List(#(key, value)) {
  case values {
    [] -> []
    [#(entry_key, value), ..rest] ->
      case same_key(entry_key, key) {
        True -> rest
        False -> [#(entry_key, value), ..remove_value(rest, key)]
      }
  }
}

fn append(items: List(a), item: a) -> List(a) {
  case items {
    [] -> [item]
    [first, ..rest] -> [first, ..append(rest, item)]
  }
}

fn regex_escape(value: String) -> String {
  value
  |> string.replace(each: "\\", with: "\\\\")
  |> string.replace(each: ".", with: "\\.")
  |> string.replace(each: "^", with: "\\^")
  |> string.replace(each: "$", with: "\\$")
  |> string.replace(each: "*", with: "\\*")
  |> string.replace(each: "+", with: "\\+")
  |> string.replace(each: "?", with: "\\?")
  |> string.replace(each: "(", with: "\\(")
  |> string.replace(each: ")", with: "\\)")
  |> string.replace(each: "[", with: "\\[")
  |> string.replace(each: "]", with: "\\]")
  |> string.replace(each: "{", with: "\\{")
  |> string.replace(each: "}", with: "\\}")
  |> string.replace(each: "|", with: "\\|")
}
