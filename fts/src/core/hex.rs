use std::error::Error as StdError;
use std::fmt::Write as _;

pub fn from_hex(
  value: &str,
) -> Result<Vec<u8>, Box<dyn StdError + Send + Sync>> {
  if value.len() < 2
    || value.len() % 2 != 0
    || &value[0..2] != "\\x"
  {
    Err("invalid hex".into())
  } else {
    (0..value.len() - 2)
      .step_by(2)
      .map(|i| {
        Ok(u8::from_str_radix(&value[i + 2..i + 4], 16)?)
      })
      .collect()
  }
}

pub fn _to_hex(value: &[u8]) -> String {
  let mut s = String::with_capacity(2 + 2 * value.len());
  write!(s, "\\x").unwrap();
  for byte in value {
    write!(s, "{:02x}", byte).unwrap();
  }
  s
}

#[cfg(test)]
#[path = "./hex_test.rs"]
mod hex_test;
