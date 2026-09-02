import gleam/string

pub fn uint8_array_to_hex(input: BitArray) -> String {
  to_hex(input, "")
}

fn to_hex(input: BitArray, output: String) -> String {
  case input {
    <<>> -> output
    <<byte, rest:bytes>> ->
      to_hex(
        rest,
        output <> nibble_to_hex(byte / 16) <> nibble_to_hex(byte % 16),
      )
    _ -> panic as "invalid Uint8Array bit width"
  }
}

pub fn uint8_array_from_hex(input: String) -> BitArray {
  from_hex(input, <<>>)
}

fn from_hex(input: String, output: BitArray) -> BitArray {
  let units = string.to_utf_codepoints(input) |> utf16_units([])
  let chunks = split_pairs(units, [], [])
  from_chunks(chunks, output)
}

fn from_chunks(chunks: List(List(Int)), output: BitArray) -> BitArray {
  case chunks {
    [] -> output
    [chunk, ..rest] -> {
      let byte = parse_int(chunk) |> to_uint8
      from_chunks(rest, <<output:bits, byte>>)
    }
  }
}

fn utf16_units(codepoints: List(UtfCodepoint), output: List(Int)) -> List(Int) {
  case codepoints {
    [] -> output
    [first, ..rest] -> {
      let codepoint = string.utf_codepoint_to_int(first)
      case codepoint <= 0xFFFF {
        True -> utf16_units(rest, append(output, [codepoint]))
        False -> {
          let offset = codepoint - 0x10000
          let high = 0xD800 + offset / 0x400
          let low = 0xDC00 + offset % 0x400
          utf16_units(rest, append(output, [high, low]))
        }
      }
    }
  }
}

fn split_pairs(
  units: List(Int),
  chunks: List(List(Int)),
  pending: List(Int),
) -> List(List(Int)) {
  case units {
    [] -> add_chunk(chunks, pending)
    [first, second, ..rest] ->
      case is_regex_dot(first) && is_regex_dot(second) {
        True -> {
          let chunks = add_chunk(chunks, pending)
          let chunks = add_chunk(chunks, [first, second])
          split_pairs(rest, chunks, [])
        }
        False -> split_pairs([second, ..rest], chunks, append(pending, [first]))
      }
    [first, ..rest] -> split_pairs(rest, chunks, append(pending, [first]))
  }
}

fn add_chunk(chunks: List(List(Int)), chunk: List(Int)) -> List(List(Int)) {
  case chunk {
    [] -> chunks
    _ -> append(chunks, [chunk])
  }
}

fn is_regex_dot(input: Int) -> Bool {
  case input {
    10 | 13 | 0x2028 | 0x2029 -> False
    _ -> True
  }
}

fn parse_int(units: List(Int)) -> Int {
  let units = trim_parse_int_whitespace(units)
  let #(negative, units) = case units {
    [45, ..rest] -> #(True, rest)
    [43, ..rest] -> #(False, rest)
    _ -> #(False, units)
  }
  let units = case units {
    [48, 120, ..rest] -> rest
    [48, 88, ..rest] -> rest
    _ -> units
  }
  let #(value, found_digit) = parse_digits(units, 0, False)
  case found_digit {
    False -> 0
    True ->
      case negative {
        True -> 0 - value
        False -> value
      }
  }
}

fn trim_parse_int_whitespace(units: List(Int)) -> List(Int) {
  case units {
    [first, ..rest] ->
      case is_parse_int_whitespace(first) {
        True -> trim_parse_int_whitespace(rest)
        False -> units
      }
    _ -> units
  }
}

fn is_parse_int_whitespace(input: Int) -> Bool {
  case input {
    9
    | 10
    | 11
    | 12
    | 13
    | 32
    | 160
    | 0x1680
    | 0x2028
    | 0x2029
    | 0x202F
    | 0x3000
    | 0xFEFF -> True
    value if value >= 0x2000 && value <= 0x200A -> True
    _ -> False
  }
}

fn parse_digits(
  units: List(Int),
  value: Int,
  found_digit: Bool,
) -> #(Int, Bool) {
  case units {
    [] -> #(value, found_digit)
    [first, ..rest] ->
      case hex_digit(first) {
        Ok(digit) -> parse_digits(rest, value * 16 + digit, True)
        Error(Nil) -> #(value, found_digit)
      }
  }
}

fn hex_digit(input: Int) -> Result(Int, Nil) {
  case input {
    value if value >= 48 && value <= 57 -> Ok(value - 48)
    value if value >= 97 && value <= 102 -> Ok(value - 87)
    value if value >= 65 && value <= 70 -> Ok(value - 55)
    _ -> Error(Nil)
  }
}

fn to_uint8(value: Int) -> Int {
  let remainder = value % 256
  case remainder < 0 {
    True -> remainder + 256
    False -> remainder
  }
}

fn append(first: List(a), second: List(a)) -> List(a) {
  case first {
    [] -> second
    [head, ..tail] -> [head, ..append(tail, second)]
  }
}

fn nibble_to_hex(value: Int) -> String {
  case value {
    0 -> "0"
    1 -> "1"
    2 -> "2"
    3 -> "3"
    4 -> "4"
    5 -> "5"
    6 -> "6"
    7 -> "7"
    8 -> "8"
    9 -> "9"
    10 -> "a"
    11 -> "b"
    12 -> "c"
    13 -> "d"
    14 -> "e"
    15 -> "f"
    _ -> panic as "invalid hexadecimal nibble"
  }
}
