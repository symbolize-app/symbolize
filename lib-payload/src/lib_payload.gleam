import gleam/float
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string

pub type JsonValue {
  JsonArray(List(JsonValue))
  JsonObject(List(#(String, JsonValue)))
  JsonBoolean(Bool)
  JsonNumber(Float)
  JsonString(String)
  JsonNull
}

// A schema-built JavaScript object has a different role from a JSON object
// token: the source transformer returns the selected object fields when
// decoding and emits a JSON object when encoding. A list of pairs is the
// smallest common Gleam representation for that dynamic object shape.
pub type Object =
  List(#(String, JsonValue))

pub type PathPart {
  Key(String)
  Index(Int)
}

pub type PayloadError {
  PayloadError(message: String, path: List(PathPart))
}

pub opaque type Transformer(value) {
  Transformer(
    from_json: fn(JsonValue, List(PathPart)) -> Result(value, PayloadError),
    to_json: fn(value, List(PathPart)) -> Result(JsonValue, PayloadError),
  )
}

pub opaque type Validator {
  Validator(
    validate: fn(JsonValue, List(PathPart)) -> Result(JsonValue, PayloadError),
  )
}

pub type Field {
  Field(name: String, validator: Validator)
}

pub type Bounds {
  Bounds(min: Float, max: Float)
}

pub type StringBounds {
  StringBounds(min: Int, max: Int)
}

pub type StringLengthMatchConfig {
  StringLengthMatchConfig(match: Regex, min: Int, max: Int)
}

pub type Regex

@external(javascript, "./regex_ffi.mjs", "new_regex")
fn new_regex(source: String, flags: String) -> Regex

@external(javascript, "./regex_ffi.mjs", "exec_regex")
fn exec_regex(regex: Regex, value: String) -> Bool

@external(javascript, "./regex_ffi.mjs", "regex_text")
fn regex_text(regex: Regex) -> String

// The source transformer methods throw PayloadError. Validation itself stays
// in Gleam; only turning the validated error into the JavaScript exception
// observed by callers crosses this runtime boundary.
@external(javascript, "./payload_ffi.mjs", "raise_payload_error")
fn raise_payload_error(message: String) -> value

pub fn regex(source: String, flags: String) -> Regex {
  new_regex(source, flags)
}

pub fn from_json(transformer: Transformer(value), input: JsonValue) -> value {
  let Transformer(from_json: from_json, ..) = transformer
  case from_json(input, []) {
    Ok(value) -> value
    Error(error) -> raise_payload_error(error_message(error))
  }
}

pub fn to_json(transformer: Transformer(value), output: value) -> JsonValue {
  let Transformer(to_json: to_json, ..) = transformer
  case to_json(output, []) {
    Ok(value) -> value
    Error(error) -> raise_payload_error(error_message(error))
  }
}

/// Builds a transformer for a project-owned value. The callbacks receive the
/// complete path to the value and return a `PayloadError` instead of throwing;
/// the top-level `from_json` and `to_json` functions preserve the source
/// exception boundary for callers.
pub fn custom(
  from_json: fn(JsonValue, List(PathPart)) -> Result(value, PayloadError),
  to_json: fn(value, List(PathPart)) -> Result(JsonValue, PayloadError),
) -> Transformer(value) {
  Transformer(from_json: from_json, to_json: to_json)
}

pub fn error_message(error: PayloadError) -> String {
  let PayloadError(message, path) = error
  message <> " at " <> format_path(path)
}

pub fn as_validator(transformer: Transformer(value)) -> Validator {
  let Transformer(from_json: from_json, to_json: to_json) = transformer
  Validator(validate: fn(input, path) {
    case from_json(input, path) {
      Error(error) -> Error(error)
      Ok(value) -> to_json(value, path)
    }
  })
}

pub fn field(name: String, validator: Validator) -> Field {
  Field(name: name, validator: validator)
}

// The TypeScript source infers a heterogeneous record from a mapped type.
// Gleam has no heterogeneous list type, so fixed-arity constructors keep the
// record boundary explicit and type checked at the call site.
pub fn object1(
  name: String,
  transformer: Transformer(field_value),
  build: fn(field_value) -> output,
  extract: fn(output) -> field_value,
) -> Transformer(output) {
  Transformer(
    from_json: fn(input, path) {
      case input {
        JsonObject(values) ->
          case find_key(values, name) {
            None -> Error(missing_key(name, path))
            Some(input) -> {
              let Transformer(from_json: from_json, ..) = transformer
              case from_json(input, build_path(Key(name), path)) {
                Ok(value) -> Ok(build(value))
                Error(error) -> Error(error)
              }
            }
          }
        _ -> Error(invalid_type("object", input, path))
      }
    },
    to_json: fn(output, path) {
      let Transformer(to_json: to_json, ..) = transformer
      case to_json(extract(output), build_path(Key(name), path)) {
        Ok(value) -> Ok(JsonObject([#(name, value)]))
        Error(error) -> Error(error)
      }
    },
  )
}

pub fn object2(
  first_name: String,
  first: Transformer(first_value),
  second_name: String,
  second: Transformer(second_value),
  build: fn(first_value, second_value) -> output,
  extract: fn(output) -> #(first_value, second_value),
) -> Transformer(output) {
  Transformer(
    from_json: fn(input, path) {
      case input {
        JsonObject(values) ->
          case find_key(values, first_name) {
            None -> Error(missing_key(first_name, path))
            Some(first_input) -> {
              let Transformer(from_json: first_from_json, ..) = first
              case
                first_from_json(first_input, build_path(Key(first_name), path))
              {
                Error(error) -> Error(error)
                Ok(first_value) -> {
                  let Transformer(from_json: second_from_json, ..) = second
                  case find_key(values, second_name) {
                    None -> Error(missing_key(second_name, path))
                    Some(second_input) ->
                      case
                        second_from_json(
                          second_input,
                          build_path(Key(second_name), path),
                        )
                      {
                        Error(error) -> Error(error)
                        Ok(second_value) -> Ok(build(first_value, second_value))
                      }
                  }
                }
              }
            }
          }
        _ -> Error(invalid_type("object", input, path))
      }
    },
    to_json: fn(output, path) {
      let #(first_value, second_value) = extract(output)
      let Transformer(to_json: first_to_json, ..) = first
      let Transformer(to_json: second_to_json, ..) = second
      case first_to_json(first_value, build_path(Key(first_name), path)) {
        Error(error) -> Error(error)
        Ok(first_output) ->
          case
            second_to_json(second_value, build_path(Key(second_name), path))
          {
            Error(error) -> Error(error)
            Ok(second_output) ->
              Ok(
                JsonObject([
                  #(first_name, first_output),
                  #(second_name, second_output),
                ]),
              )
          }
      }
    },
  )
}

pub fn null_or(transformer: Transformer(value)) -> Transformer(Option(value)) {
  let Transformer(from_json: from_json, to_json: to_json) = transformer
  Transformer(
    from_json: fn(input, path) {
      case input {
        JsonNull -> Ok(None)
        _ -> {
          case from_json(input, path) {
            Ok(value) -> Ok(Some(value))
            Error(error) -> Error(error)
          }
        }
      }
    },
    to_json: fn(output, path) {
      case output {
        None -> Ok(JsonNull)
        Some(value) -> to_json(value, path)
      }
    },
  )
}

pub fn object(fields: List(Field)) -> Transformer(Object) {
  Transformer(
    from_json: fn(input, path) {
      case input {
        JsonObject(values) -> validate_fields(fields, values, path, [])
        _ -> Error(invalid_type("object", input, path))
      }
    },
    to_json: fn(output, path) {
      case validate_fields(fields, output, path, []) {
        Ok(values) -> Ok(JsonObject(values))
        Error(error) -> Error(error)
      }
    },
  )
}

pub fn array(transformer: Transformer(value)) -> Transformer(List(value)) {
  let Transformer(from_json: from_json, to_json: to_json) = transformer
  Transformer(
    from_json: fn(input, path) {
      case input {
        JsonArray(items) -> from_array(items, from_json, path, 0, [])
        _ -> Error(invalid_type("array", input, path))
      }
    },
    to_json: fn(output, path) { to_array(output, to_json, path, 0, []) },
  )
}

pub fn string() -> Transformer(String) {
  Transformer(
    from_json: fn(input, path) {
      case input {
        JsonString(value) -> Ok(value)
        _ -> Error(invalid_type("string", input, path))
      }
    },
    to_json: fn(output, _path) { Ok(JsonString(output)) },
  )
}

pub fn string_length(bounds: StringBounds) -> Transformer(String) {
  let base = string()
  let Transformer(from_json: from_json, to_json: to_json) = base
  Transformer(
    from_json: fn(input, path) {
      case from_json(input, path) {
        Error(error) -> Error(error)
        Ok(value) -> {
          case check_string_length(value, bounds, path) {
            Ok(_) -> Ok(value)
            Error(error) -> Error(error)
          }
        }
      }
    },
    to_json: fn(output, path) {
      case check_string_length(output, bounds, path) {
        Error(error) -> Error(error)
        Ok(_) -> to_json(output, path)
      }
    },
  )
}

pub fn string_option(options: List(String)) -> Transformer(String) {
  let base = string()
  let Transformer(from_json: from_json, to_json: to_json) = base
  Transformer(
    from_json: fn(input, path) {
      case from_json(input, path) {
        Error(error) -> Error(error)
        Ok(value) -> {
          case list.contains(options, value) {
            True -> Ok(value)
            False -> Error(invalid_option(options, path))
          }
        }
      }
    },
    to_json: fn(output, path) {
      case list.contains(options, output) {
        True -> to_json(output, path)
        False -> Error(invalid_option(options, path))
      }
    },
  )
}

pub fn string_length_match(
  config: StringLengthMatchConfig,
) -> Transformer(String) {
  let StringLengthMatchConfig(matcher, min, max) = config
  let bounds = StringBounds(min: min, max: max)
  let base = string_length(bounds)
  let Transformer(from_json: from_json, to_json: to_json) = base
  Transformer(
    from_json: fn(input, path) {
      case from_json(input, path) {
        Error(error) -> Error(error)
        Ok(value) -> check_match(value, matcher, path)
      }
    },
    to_json: fn(output, path) {
      case check_match(output, matcher, path) {
        Error(error) -> Error(error)
        Ok(value) -> to_json(value, path)
      }
    },
  )
}

pub fn string_enum(mapping: List(#(String, String))) -> Transformer(String) {
  string_option(list.map(mapping, fn(pair) { pair.1 }))
}

pub fn number() -> Transformer(Float) {
  Transformer(
    from_json: fn(input, path) {
      case input {
        JsonNumber(value) -> Ok(value)
        _ -> Error(invalid_type("number", input, path))
      }
    },
    to_json: fn(output, _path) { Ok(JsonNumber(output)) },
  )
}

pub fn number_range(bounds: Bounds) -> Transformer(Float) {
  let base = number()
  let Transformer(from_json: from_json, to_json: to_json) = base
  Transformer(
    from_json: fn(input, path) {
      case from_json(input, path) {
        Error(error) -> Error(error)
        Ok(value) -> {
          case check_number_range(value, bounds, path) {
            Ok(_) -> Ok(value)
            Error(error) -> Error(error)
          }
        }
      }
    },
    to_json: fn(output, path) {
      case check_number_range(output, bounds, path) {
        Error(error) -> Error(error)
        Ok(_) -> to_json(output, path)
      }
    },
  )
}

pub fn integer_range(bounds: Bounds) -> Transformer(Float) {
  let base = number_range(bounds)
  let Transformer(from_json: from_json, to_json: to_json) = base
  Transformer(
    from_json: fn(input, path) {
      case from_json(input, path) {
        Error(error) -> Error(error)
        Ok(value) -> check_integer(value, path)
      }
    },
    to_json: fn(output, path) {
      case check_integer(output, path) {
        Error(error) -> Error(error)
        Ok(value) -> to_json(value, path)
      }
    },
  )
}

pub fn boolean() -> Transformer(Bool) {
  Transformer(
    from_json: fn(input, path) {
      case input {
        JsonBoolean(value) -> Ok(value)
        _ -> Error(invalid_type("boolean", input, path))
      }
    },
    to_json: fn(output, _path) { Ok(JsonBoolean(output)) },
  )
}

pub fn build_path(part: PathPart, parent: List(PathPart)) -> List(PathPart) {
  list.append(parent, [part])
}

pub fn get_type_name(input: JsonValue) -> String {
  case input {
    JsonNull -> "null"
    JsonArray(_) -> "array"
    JsonObject(_) -> "object"
    JsonBoolean(_) -> "boolean"
    JsonNumber(_) -> "number"
    JsonString(_) -> "string"
  }
}

fn validate_fields(
  fields: List(Field),
  values: List(#(String, JsonValue)),
  path: List(PathPart),
  output: List(#(String, JsonValue)),
) -> Result(Object, PayloadError) {
  case fields {
    [] -> Ok(list.reverse(output))
    [Field(name, Validator(validate)), ..rest] -> {
      case find_key(values, name) {
        None -> Error(missing_key(name, path))
        Some(value) -> {
          let field_path = build_path(Key(name), path)
          case validate(value, field_path) {
            Error(error) -> Error(error)
            Ok(value) ->
              validate_fields(rest, values, path, [#(name, value), ..output])
          }
        }
      }
    }
  }
}

fn from_array(
  items: List(JsonValue),
  from_json: fn(JsonValue, List(PathPart)) -> Result(value, PayloadError),
  path: List(PathPart),
  index: Int,
  output: List(value),
) -> Result(List(value), PayloadError) {
  case items {
    [] -> Ok(list.reverse(output))
    [item, ..rest] -> {
      case from_json(item, build_path(Index(index), path)) {
        Error(error) -> Error(error)
        Ok(value) ->
          from_array(rest, from_json, path, index + 1, [value, ..output])
      }
    }
  }
}

fn to_array(
  items: List(value),
  to_json: fn(value, List(PathPart)) -> Result(JsonValue, PayloadError),
  path: List(PathPart),
  index: Int,
  output: List(JsonValue),
) -> Result(JsonValue, PayloadError) {
  case items {
    [] -> Ok(JsonArray(list.reverse(output)))
    [item, ..rest] -> {
      case to_json(item, build_path(Index(index), path)) {
        Error(error) -> Error(error)
        Ok(value) -> to_array(rest, to_json, path, index + 1, [value, ..output])
      }
    }
  }
}

fn check_string_length(
  value: String,
  bounds: StringBounds,
  path: List(PathPart),
) -> Result(Nil, PayloadError) {
  let length = utf16_length(value)
  let StringBounds(min, max) = bounds
  case length < min {
    True ->
      Error(PayloadError(
        "Invalid string (too short, min " <> int.to_string(min) <> ")",
        path,
      ))
    False ->
      case length > max {
        True ->
          Error(PayloadError(
            "Invalid string (too long, max " <> int.to_string(max) <> ")",
            path,
          ))
        False -> Ok(Nil)
      }
  }
}

fn check_match(
  value: String,
  matcher: Regex,
  path: List(PathPart),
) -> Result(String, PayloadError) {
  case exec_regex(matcher, value) {
    True -> Ok(value)
    False ->
      Error(PayloadError(
        "Invalid string match (for "
          <> string.inspect(regex_text(matcher))
          <> ")",
        path,
      ))
  }
}

fn check_number_range(
  value: Float,
  bounds: Bounds,
  path: List(PathPart),
) -> Result(Nil, PayloadError) {
  let Bounds(min, max) = bounds
  case value <. min {
    True ->
      Error(PayloadError(
        "Invalid number (too small, min " <> format_float(min) <> ")",
        path,
      ))
    False ->
      case value >. max {
        True ->
          Error(PayloadError(
            "Invalid number (too large, max " <> format_float(max) <> ")",
            path,
          ))
        False -> Ok(Nil)
      }
  }
}

fn check_integer(
  value: Float,
  path: List(PathPart),
) -> Result(Float, PayloadError) {
  // The source uses `(value | 0) !== value`, so integer values must also fit
  // JavaScript's signed 32-bit bitwise range. A floor check alone would
  // incorrectly accept values such as 2**31 and 2**32.
  case
    value == float.floor(value)
    && value >=. -2_147_483_648.0
    && value <=. 2_147_483_647.0
  {
    True -> Ok(value)
    False ->
      Error(PayloadError(
        "Invalid integer (includes fractional component)",
        path,
      ))
  }
}

fn invalid_type(
  expected: String,
  input: JsonValue,
  path: List(PathPart),
) -> PayloadError {
  PayloadError(
    "Invalid " <> expected <> " (wrong type " <> get_type_name(input) <> ")",
    path,
  )
}

fn missing_key(name: String, path: List(PathPart)) -> PayloadError {
  PayloadError(
    "Invalid object (missing key " <> string.inspect(name) <> ")",
    path,
  )
}

fn invalid_option(options: List(String), path: List(PathPart)) -> PayloadError {
  let rendered = options |> list.map(string.inspect) |> string.join(with: " | ")
  PayloadError("Invalid string option (not " <> rendered <> ")", path)
}

fn find_key(
  values: List(#(String, JsonValue)),
  key: String,
) -> Option(JsonValue) {
  case values {
    [] -> None
    [#(current, value), ..rest] ->
      case current == key {
        True -> Some(value)
        False -> find_key(rest, key)
      }
  }
}

fn format_path(path: List(PathPart)) -> String {
  "(root)" <> format_path_parts(path)
}

fn format_path_parts(path: List(PathPart)) -> String {
  case path {
    [] -> ""
    [Key(key), ..rest] -> {
      let prefix = case identifier_key(key) {
        True -> "." <> key
        False -> "[" <> string.inspect(key) <> "]"
      }
      prefix <> format_path_parts(rest)
    }
    [Index(index), ..rest] ->
      "[" <> int_to_string(index) <> "]" <> format_path_parts(rest)
  }
}

fn identifier_key(key: String) -> Bool {
  case string.to_utf_codepoints(key) {
    [] -> False
    [first, ..rest] -> {
      let codepoint = string.utf_codepoint_to_int(first)
      ascii_identifier_start(codepoint)
      && list.all(rest, fn(item) {
        !ascii_whitespace(string.utf_codepoint_to_int(item))
      })
    }
  }
}

fn ascii_identifier_start(codepoint: Int) -> Bool {
  case codepoint >= 65 && codepoint <= 90 {
    True -> True
    False ->
      case codepoint >= 97 && codepoint <= 122 {
        True -> True
        False -> codepoint == 95
      }
  }
}

fn ascii_whitespace(codepoint: Int) -> Bool {
  codepoint == 9
  || codepoint == 10
  || codepoint == 11
  || codepoint == 12
  || codepoint == 13
  || codepoint == 32
  || ecmascript_whitespace(codepoint)
}

// The source path formatter uses JavaScript's `\S` character class. Keep the
// corresponding non-ASCII whitespace set in Gleam rather than changing the
// path contract at this runtime boundary.
fn ecmascript_whitespace(codepoint: Int) -> Bool {
  codepoint == 160
  || codepoint == 5760
  || codepoint >= 8192
  && codepoint <= 8202
  || codepoint == 8239
  || codepoint == 8287
  || codepoint == 12_288
  || codepoint == 65_279
}

fn utf16_length(value: String) -> Int {
  value
  |> string.to_utf_codepoints
  |> list.fold(0, fn(total, codepoint) {
    case string.utf_codepoint_to_int(codepoint) > 65_535 {
      True -> total + 2
      False -> total + 1
    }
  })
}

fn int_to_string(value: Int) -> String {
  int.to_string(value)
}

fn format_float(value: Float) -> String {
  let output = float.to_string(value)
  case string.ends_with(output, ".0") {
    True -> string.drop_end(output, 2)
    False -> output
  }
}
