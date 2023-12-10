mod core;

use crate::core::hex::ToHex;

#[allow(clippy::print_stdout)]
fn main() {
  println!("Hello, {} World!", [0x00, 0x20, 0x0a, 0xff].to_hex());
}
