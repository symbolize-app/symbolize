use anyhow::Result;
use anyhow::anyhow;
use std::fmt::Write as _;

pub trait FromHex {
  fn from_hex(value: &str) -> Result<Self>
  where
    Self: Sized;
}

impl FromHex for Vec<u8> {
  fn from_hex(value: &str) -> Result<Self> {
    if value.len() % 2 != 0 {
      Err(anyhow!("invalid hex size"))
    } else {
      (0 .. value.len())
        .step_by(2)
        .map(|i| Ok(u8::from_str_radix(&value[i .. i + 2], 16)?))
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
      write!(s, "{byte:02x}").unwrap();
    }
    s
  }
}

#[cfg(test)]
#[path = "./lib_test.rs"]
mod lib_test;
