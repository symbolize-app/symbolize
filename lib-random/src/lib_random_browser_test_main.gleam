import gleam/io
import gleam/list
import lib_random

pub fn main() {
  let random = lib_random.random()
  let #(random, value) = lib_random.number(random)
  assert value >=. 0.0
  assert value <. 1.0

  let #(_, bytes) = lib_random.crypto_bits(random, 256)
  assert bit_array_length(bytes) == 256

  io.println("lib-random Chromium Web Crypto/Math FFI passed")
}

fn bit_array_length(value: BitArray) -> Int {
  list.length(bit_array_bytes(value)) * 8
}

fn bit_array_bytes(value: BitArray) -> List(Int) {
  case value {
    <<byte, rest:bytes>> -> [byte, ..bit_array_bytes(rest)]
    <<>> -> []
    _ -> panic as "expected a whole number of bytes"
  }
}
