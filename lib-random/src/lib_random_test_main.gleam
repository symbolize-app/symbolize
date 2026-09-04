import gleam/io
import gleam/list
import lib_random

pub fn run(done: fn() -> Nil) {
  let random = lib_random.seeded(1_616_952_581_493)
  let #(random, first) = lib_random.number(random)
  let #(_, second) = lib_random.number(random)
  let #(_, bytes) =
    lib_random.crypto_bits(lib_random.seeded(1_616_952_581_493), 256)

  assert first == 0.32524261344224215
  assert second == 0.26809328328818083
  assert bytes
    == <<
      148,
      25,
      67,
      83,
      236,
      194,
      161,
      68,
      133,
      3,
      225,
      39,
      117,
      184,
      162,
      13,
      201,
      86,
      169,
      202,
      38,
      239,
      16,
      242,
      250,
      147,
      11,
      231,
      147,
      27,
      250,
      116,
    >>

  let context = lib_random.new_context(lib_random.seeded(1_616_952_581_493))
  let #(context, id_hex) = lib_random.request_id_hex(context)
  assert id_hex
    == "94194353ecc2a1448503e12775b8a20dc956a9ca26ef10f2fa930be7931bfa74"

  let #(_, id) = lib_random.request_id(context)
  assert bit_array_length(id) == 256

  let system = lib_random.random()
  let #(system, system_number) = lib_random.number(system)
  assert system_number >=. 0.0
  assert system_number <. 1.0
  let #(_, system_bytes) = lib_random.crypto_bits(system, 32)
  assert bit_array_length(system_bytes) == 32

  let custom = lib_random.custom(fn(_bits) { <<0>> }, fn() { 0.5 })
  let #(custom, custom_number) = lib_random.number(custom)
  assert custom_number == 0.5
  let #(_, custom_bytes) = lib_random.crypto_bits(custom, 8)
  assert custom_bytes == <<0>>

  io.println("lib-random Gleam parity tests passed")
  done()
}

pub fn main() {
  run(fn() { Nil })
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
