import gleam/int
import gleam/list
import lib_hex

const word_count = 624

const last_word = 623

const middle_word = 396

const split = 227

const matrix_a = 0x9908B0DF

const upper_mask = 0x80000000

const lower_mask = 0x7FFFFFFF

const word_modulus = 4_294_967_296

pub type Context {
  Context(random: Random)
}

pub opaque type Random {
  System(SystemRandom)
  Seeded(Mersenne)
  Custom(crypto_bits: fn(Int) -> BitArray, number: fn() -> Float)
}

pub type SystemRandom

type Mersenne {
  Mersenne(state: List(Int), next: Int)
}

@external(javascript, "./random_ffi.mjs", "new_system_random")
fn new_system_random() -> SystemRandom

@external(javascript, "./random_ffi.mjs", "crypto_bits")
fn system_crypto_bytes(random: SystemRandom, byte_count: Int) -> BitArray

@external(javascript, "./random_ffi.mjs", "math_random")
fn system_number(random: SystemRandom) -> Float

pub fn random() -> Random {
  System(new_system_random())
}

pub fn seeded(seed: Int) -> Random {
  let state = initialize(seed) |> twist
  Seeded(Mersenne(state: state, next: 0))
}

pub fn custom(
  crypto_bits: fn(Int) -> BitArray,
  number: fn() -> Float,
) -> Random {
  Custom(crypto_bits: crypto_bits, number: number)
}

pub fn new_context(random: Random) -> Context {
  Context(random: random)
}

pub fn number(random: Random) -> #(Random, Float) {
  case random {
    System(system) -> #(random, system_number(system))
    Custom(number: number, ..) -> #(random, number())
    Seeded(mersenne) -> {
      let #(mersenne, value) = random_int32(mersenne)
      let value = int.to_float(value) /. 4_294_967_296.0
      #(Seeded(mersenne), value)
    }
  }
}

pub fn crypto_bits(random: Random, bits: Int) -> #(Random, BitArray) {
  case random {
    System(system) -> #(random, system_crypto_bytes(system, bits / 8))
    Custom(crypto_bits: crypto_bits, ..) -> #(random, crypto_bits(bits))
    Seeded(mersenne) -> {
      let byte_count = bits / 8
      let #(mersenne, output) = seeded_crypto_bits(mersenne, byte_count, <<>>)
      #(Seeded(mersenne), output)
    }
  }
}

pub fn request_id(context: Context) -> #(Context, BitArray) {
  let Context(random) = context
  let #(random, value) = crypto_bits(random, 256)
  #(Context(random), value)
}

pub fn request_id_hex(context: Context) -> #(Context, String) {
  let #(context, value) = request_id(context)
  #(context, lib_hex.uint8_array_to_hex(value))
}

fn initialize(seed: Int) -> List(Int) {
  initialize_loop(1, u32(seed), [u32(seed)])
}

fn initialize_loop(index: Int, previous: Int, state: List(Int)) -> List(Int) {
  case index >= word_count {
    True -> list.reverse(state)
    False -> {
      let mixed =
        u32(int.bitwise_exclusive_or(previous, logical_right(previous, 30)))
      let high = logical_right(u32(int.bitwise_and(mixed, 0xFFFF0000)), 16)
      let low = int.bitwise_and(mixed, 0x0000FFFF)
      let value =
        u32(
          int.bitwise_and(high * 1_812_433_253, 0xFFFF)
          * 65_536
          + low
          * 1_812_433_253
          + index,
        )
      initialize_loop(index + 1, value, [value, ..state])
    }
  }
}

fn twist(state: List(Int)) -> List(Int) {
  let state = twist_first(state, 0)
  twist_second(state, split)
}

fn twist_first(state: List(Int), index: Int) -> List(Int) {
  case index >= split {
    True -> state
    False -> {
      let bits = combine_bits(at(state, index), at(state, index + 1))
      let value = next_word(at(state, index + middle_word + 1), bits)
      twist_first(replace_at(state, index, value), index + 1)
    }
  }
}

fn twist_second(state: List(Int), index: Int) -> List(Int) {
  case index >= last_word {
    True -> {
      let bits = combine_bits(at(state, last_word), at(state, 0))
      let value = next_word(at(state, middle_word), bits)
      replace_at(state, last_word, value)
    }
    False -> {
      let bits = combine_bits(at(state, index), at(state, index + 1))
      let value = next_word(at(state, index - split), bits)
      twist_second(replace_at(state, index, value), index + 1)
    }
  }
}

fn combine_bits(current: Int, next: Int) -> Int {
  u32(int.bitwise_or(
    int.bitwise_and(current, upper_mask),
    int.bitwise_and(next, lower_mask),
  ))
}

fn next_word(source: Int, bits: Int) -> Int {
  let matrix = case int.bitwise_and(bits, 1) {
    0 -> 0
    _ -> matrix_a
  }

  u32(int.bitwise_exclusive_or(
    int.bitwise_exclusive_or(source, logical_right(bits, 1)),
    matrix,
  ))
}

fn random_int32(mersenne: Mersenne) -> #(Mersenne, Int) {
  let #(state, next) = case mersenne.next >= word_count {
    True -> #(twist(mersenne.state), 0)
    False -> #(mersenne.state, mersenne.next)
  }
  let value = at(state, next)
  let value = temper(value)
  #(Mersenne(state: state, next: next + 1), value)
}

fn temper(value: Int) -> Int {
  let value = u32(int.bitwise_exclusive_or(value, logical_right(value, 11)))
  let value =
    u32(int.bitwise_exclusive_or(
      value,
      int.bitwise_and(int.bitwise_shift_left(value, 7), 0x9D2C5680),
    ))
  let value =
    u32(int.bitwise_exclusive_or(
      value,
      int.bitwise_and(int.bitwise_shift_left(value, 15), 0xEFC60000),
    ))
  u32(int.bitwise_exclusive_or(value, logical_right(value, 18)))
}

fn seeded_crypto_bits(
  mersenne: Mersenne,
  byte_count: Int,
  output: BitArray,
) -> #(Mersenne, BitArray) {
  case byte_count <= 0 {
    True -> #(mersenne, output)
    False -> {
      let #(mersenne, word) = random_int32(mersenne)
      let word_byte_count = case byte_count < 4 {
        True -> byte_count
        False -> 4
      }
      let output = append_word(output, word, 0, word_byte_count)
      seeded_crypto_bits(mersenne, byte_count - word_byte_count, output)
    }
  }
}

fn append_word(
  output: BitArray,
  word: Int,
  offset: Int,
  byte_count: Int,
) -> BitArray {
  case offset >= byte_count {
    True -> output
    False -> {
      let shift = offset * 8
      let byte = int.bitwise_and(logical_right(word, shift), 255)
      append_word(<<output:bits, byte:8>>, word, offset + 1, byte_count)
    }
  }
}

fn u32(value: Int) -> Int {
  let remainder = value % word_modulus
  case remainder < 0 {
    True -> remainder + word_modulus
    False -> remainder
  }
}

fn logical_right(value: Int, shift: Int) -> Int {
  value
  / case shift {
    0 -> 1
    1 -> 2
    7 -> 128
    8 -> 256
    11 -> 2048
    16 -> 65_536
    18 -> 262_144
    24 -> 16_777_216
    30 -> 1_073_741_824
    _ -> panic as "unsupported logical shift"
  }
}

fn at(values: List(Int), index: Int) -> Int {
  case values {
    [head, ..tail] ->
      case index {
        0 -> head
        _ -> at(tail, index - 1)
      }
    [] -> panic as "Mersenne Twister state index out of bounds"
  }
}

fn replace_at(values: List(Int), index: Int, value: Int) -> List(Int) {
  case values {
    [head, ..tail] ->
      case index {
        0 -> [value, ..tail]
        _ -> [head, ..replace_at(tail, index - 1, value)]
      }
    [] -> panic as "Mersenne Twister state index out of bounds"
  }
}
