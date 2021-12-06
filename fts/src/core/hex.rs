use std::error::Error as StdError;
use std::fmt::Write as _;

pub trait FromHex {
  fn from_hex(
    value: &str,
  ) -> Result<Self, Box<dyn StdError + Send + Sync>>
  where
    Self: Sized;
}

impl FromHex for Vec<u8> {
  fn from_hex(
    value: &str,
  ) -> Result<Self, Box<dyn StdError + Send + Sync>> {
    if value.len() % 2 != 0 {
      Err("invalid hex size".into())
    } else {
      (0..value.len())
        .step_by(2)
        .map(|i| {
          Ok(u8::from_str_radix(&value[i..i + 2], 16)?)
        })
        .collect()
    }
  }
}

pub trait ToHex {
  fn to_hex(&self) -> String;
}

impl ToHex for [u8] {
  fn to_hex(&self) -> String {
    let mut s = String::with_capacity(2 + 2 * self.len());
    for byte in self {
      write!(s, "{:02x}", byte).unwrap();
    }
    s
  }
}

#[cfg(test)]
#[path = "./hex_test.rs"]
mod hex_test;
