import gleam/io
import lib_payload as payload

@external(javascript, "./payload_ffi.mjs", "call_and_catch")
fn call_and_catch(callback: fn() -> Nil) -> String

type User {
  User(age: Float, name: String)
}

type Label {
  Label(String)
}

pub fn run(done: fn() -> Nil) {
  let empty = payload.object([])
  let nested =
    payload.object([
      payload.field("x", payload.as_validator(payload.null_or(empty))),
      payload.field(
        "y",
        payload.as_validator(payload.null_or(payload.object([]))),
      ),
    ])
  assert payload.from_json(
      nested,
      payload.JsonObject([
        #("x", payload.JsonNull),
        #("y", payload.JsonObject([])),
      ]),
    )
    == [
      #("x", payload.JsonNull),
      #("y", payload.JsonObject([])),
    ]
  assert payload.to_json(nested, [
      #("x", payload.JsonNull),
      #("y", payload.JsonObject([])),
    ])
    == payload.JsonObject([
      #("x", payload.JsonNull),
      #("y", payload.JsonObject([])),
    ])

  let object_validator =
    payload.object([
      payload.field("x", payload.as_validator(payload.object([]))),
    ])
  assert payload.from_json(
      object_validator,
      payload.JsonObject([
        #("x", payload.JsonObject([])),
        #("extra", payload.JsonNull),
      ]),
    )
    == [#("x", payload.JsonObject([]))]
  assert_error(
    fn() {
      let _ = payload.from_json(object_validator, payload.JsonNull)
    },
    "Invalid object (wrong type null) at (root)",
  )
  assert_error(
    fn() {
      let _ = payload.from_json(object_validator, payload.JsonObject([]))
    },
    "Invalid object (missing key \"x\") at (root)",
  )
  assert payload.to_json(object_validator, [
      #("x", payload.JsonObject([])),
      #("extra", payload.JsonNull),
    ])
    == payload.JsonObject([#("x", payload.JsonObject([]))])

  let nested_missing =
    payload.object([
      payload.field(
        "space y",
        payload.as_validator(
          payload.object([
            payload.field("z", payload.as_validator(payload.object([]))),
          ]),
        ),
      ),
    ])
  assert_error(
    fn() {
      let _ =
        payload.from_json(
          nested_missing,
          payload.JsonObject([#("space y", payload.JsonObject([]))]),
        )
    },
    "Invalid object (missing key \"z\") at (root)[\"space y\"]",
  )

  let unicode_space_key =
    payload.object([
      payload.field(
        "space\u{00a0}y",
        payload.as_validator(
          payload.object([
            payload.field("z", payload.as_validator(payload.object([]))),
          ]),
        ),
      ),
    ])
  assert_error(
    fn() {
      let _ =
        payload.from_json(
          unicode_space_key,
          payload.JsonObject([#("space\u{00a0}y", payload.JsonObject([]))]),
        )
    },
    "Invalid object (missing key \"z\") at (root)[\"space\u{00a0}y\"]",
  )

  let array_validator = payload.array(payload.object([]))
  assert payload.from_json(
      array_validator,
      payload.JsonArray([payload.JsonObject([])]),
    )
    == [[]]
  assert payload.to_json(array_validator, [[]])
    == payload.JsonArray([payload.JsonObject([])])
  assert_error(
    fn() {
      let _ = payload.from_json(array_validator, payload.JsonNull)
    },
    "Invalid array (wrong type null) at (root)",
  )

  let bounded = payload.string_length(payload.StringBounds(min: 2, max: 5))
  assert payload.from_json(bounded, payload.JsonString("abcde")) == "abcde"
  assert_error(
    fn() {
      let _ = payload.from_json(bounded, payload.JsonArray([]))
    },
    "Invalid string (wrong type array) at (root)",
  )
  assert_error(
    fn() {
      let _ = payload.from_json(bounded, payload.JsonString("a"))
    },
    "Invalid string (too short, min 2) at (root)",
  )
  assert_error(
    fn() {
      let _ = payload.from_json(bounded, payload.JsonString("abcdef"))
    },
    "Invalid string (too long, max 5) at (root)",
  )
  assert payload.from_json(bounded, payload.JsonString("😀")) == "😀"
  assert payload.to_json(bounded, "abc") == payload.JsonString("abc")

  let matched =
    payload.string_length_match(payload.StringLengthMatchConfig(
      match: payload.regex("^a+$", ""),
      min: 2,
      max: 5,
    ))
  assert payload.from_json(matched, payload.JsonString("aaa")) == "aaa"
  assert_error(
    fn() {
      let _ = payload.from_json(matched, payload.JsonString("abc"))
    },
    "Invalid string match (for \"/^a+$/\") at (root)",
  )
  assert_error(
    fn() {
      let _ = payload.to_json(matched, "abc")
    },
    "Invalid string match (for \"/^a+$/\") at (root)",
  )

  let stateful = payload.regex("a+", "g")
  let stateful_match =
    payload.string_length_match(payload.StringLengthMatchConfig(
      match: stateful,
      min: 0,
      max: 2,
    ))
  assert payload.from_json(stateful_match, payload.JsonString("aa")) == "aa"
  assert_error(
    fn() {
      let _ = payload.from_json(stateful_match, payload.JsonString("aa"))
    },
    "Invalid string match (for \"/a+/g\") at (root)",
  )
  assert payload.from_json(stateful_match, payload.JsonString("aa")) == "aa"

  let options = payload.string_option(["abc", "xyz"])
  assert payload.from_json(options, payload.JsonString("abc")) == "abc"
  assert_error(
    fn() {
      let _ = payload.from_json(options, payload.JsonNumber(2.0))
    },
    "Invalid string (wrong type number) at (root)",
  )
  assert_error(
    fn() {
      let _ = payload.from_json(options, payload.JsonString("a"))
    },
    "Invalid string option (not \"abc\" | \"xyz\") at (root)",
  )
  assert_error(
    fn() {
      let _ = payload.to_json(options, "a")
    },
    "Invalid string option (not \"abc\" | \"xyz\") at (root)",
  )

  let enum = payload.string_enum([#("x", "x"), #("y", "y2")])
  assert payload.from_json(enum, payload.JsonString("y2")) == "y2"
  assert_error(
    fn() {
      let _ = payload.from_json(enum, payload.JsonString("y"))
    },
    "Invalid string option (not \"x\" | \"y2\") at (root)",
  )

  let numbers = payload.number_range(payload.Bounds(min: 1.0, max: 3.0))
  assert payload.from_json(numbers, payload.JsonNumber(2.0)) == 2.0
  assert_error(
    fn() {
      let _ = payload.from_json(numbers, payload.JsonNumber(4.0))
    },
    "Invalid number (too large, max 3) at (root)",
  )
  assert_error(
    fn() {
      let _ = payload.to_json(numbers, 4.0)
    },
    "Invalid number (too large, max 3) at (root)",
  )

  let booleans = payload.boolean()
  assert payload.from_json(booleans, payload.JsonBoolean(True)) == True
  assert_error(
    fn() {
      let _ = payload.from_json(booleans, payload.JsonString("true"))
    },
    "Invalid boolean (wrong type string) at (root)",
  )

  let integers =
    payload.integer_range(payload.Bounds(
      min: -5_000_000_000.0,
      max: 5_000_000_000.0,
    ))
  assert payload.from_json(integers, payload.JsonNumber(2_147_483_647.0))
    == 2_147_483_647.0
  assert payload.from_json(integers, payload.JsonNumber(-2_147_483_648.0))
    == -2_147_483_648.0
  assert_error(
    fn() {
      let _ = payload.from_json(integers, payload.JsonNumber(2_147_483_648.0))
    },
    "Invalid integer (includes fractional component) at (root)",
  )
  assert_error(
    fn() {
      let _ = payload.from_json(integers, payload.JsonNumber(4_294_967_296.0))
    },
    "Invalid integer (includes fractional component) at (root)",
  )

  assert payload.get_type_name(payload.JsonNull) == "null"
  assert payload.get_type_name(payload.JsonString("a")) == "string"
  assert payload.get_type_name(payload.JsonNumber(1.0)) == "number"
  assert payload.get_type_name(payload.JsonArray([])) == "array"
  assert payload.get_type_name(payload.JsonObject([])) == "object"
  assert payload.get_type_name(payload.JsonBoolean(True)) == "boolean"

  let user =
    payload.object2(
      "age",
      payload.number(),
      "name",
      payload.string(),
      fn(age, name) { User(age: age, name: name) },
      fn(user) { #(user.age, user.name) },
    )
  assert payload.from_json(
      user,
      payload.JsonObject([
        #("age", payload.JsonNumber(42.0)),
        #("name", payload.JsonString("Ada")),
        #("extra", payload.JsonBoolean(True)),
      ]),
    )
    == User(age: 42.0, name: "Ada")
  assert payload.to_json(user, User(age: 42.0, name: "Ada"))
    == payload.JsonObject([
      #("age", payload.JsonNumber(42.0)),
      #("name", payload.JsonString("Ada")),
    ])
  assert_error(
    fn() {
      let _ =
        payload.from_json(
          user,
          payload.JsonObject([#("age", payload.JsonString("forty-two"))]),
        )
    },
    "Invalid number (wrong type string) at (root).age",
  )
  assert_error(
    fn() {
      let _ =
        payload.from_json(
          user,
          payload.JsonObject([#("age", payload.JsonNumber(42.0))]),
        )
    },
    "Invalid object (missing key \"name\") at (root)",
  )

  let label =
    payload.custom(
      fn(input, path) {
        case input {
          payload.JsonString(value) -> Ok(Label(value))
          _ -> Error(payload.PayloadError("Invalid label", path))
        }
      },
      fn(output, _path) {
        let Label(value) = output
        Ok(payload.JsonString(value))
      },
    )
  let named_label =
    payload.object1("name", label, fn(value) { value }, fn(value) { value })
  assert payload.from_json(
      named_label,
      payload.JsonObject([#("name", payload.JsonString("Ada"))]),
    )
    == Label("Ada")
  assert payload.to_json(named_label, Label("Ada"))
    == payload.JsonObject([#("name", payload.JsonString("Ada"))])
  assert_error(
    fn() {
      let _ =
        payload.from_json(
          named_label,
          payload.JsonObject([#("name", payload.JsonNumber(1.0))]),
        )
    },
    "Invalid label at (root).name",
  )

  io.println("lib-payload Gleam parity tests passed")
  done()
}

pub fn main() {
  run(fn() { Nil })
}

fn assert_error(callback: fn() -> value, expected: String) {
  assert call_and_catch(fn() {
      let _ = callback()
      Nil
    })
    == "PayloadError:" <> expected
}
