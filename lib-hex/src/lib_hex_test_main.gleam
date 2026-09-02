import gleam/io
import lib_hex as hex

pub fn run(done: fn() -> Nil) {
  assert hex.uint8_array_to_hex(<<0x00, 0x23, 0xA0, 0xFF>>) == "0023a0ff"
  assert hex.uint8_array_to_hex(<<>>) == ""
  assert hex.uint8_array_from_hex("0023a0ff") == <<0x00, 0x23, 0xA0, 0xFF>>
  assert hex.uint8_array_from_hex("") == <<>>
  assert hex.uint8_array_from_hex("0") == <<0>>
  assert hex.uint8_array_from_hex("a") == <<10>>
  assert hex.uint8_array_from_hex("1z") == <<1>>
  assert hex.uint8_array_from_hex("z1") == <<0>>
  assert hex.uint8_array_from_hex("123") == <<18, 3>>
  assert hex.uint8_array_from_hex("gg") == <<0>>
  assert hex.uint8_array_from_hex("😀") == <<0>>
  assert hex.uint8_array_from_hex("😀a") == <<0, 10>>
  assert hex.uint8_array_from_hex("a😀") == <<10, 0>>
  assert hex.uint8_array_from_hex("\n12") == <<0, 18>>
  assert hex.uint8_array_from_hex("1\n2") == <<1>>
  assert hex.uint8_array_from_hex("123\n4") == <<18, 3>>
  assert hex.uint8_array_from_hex("-1") == <<255>>
  assert hex.uint8_array_from_hex("0x10") == <<0, 16>>
  io.println("lib-hex Gleam parity tests passed")
  done()
}

pub fn main() {
  run(fn() { Nil })
}
