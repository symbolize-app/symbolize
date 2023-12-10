use super::*;

#[test]
fn from_hex_basic() {
  assert_eq!(
    vec![0x00, 0x20, 0x0a, 0xff],
    Vec::from_hex("00200AFF").unwrap()
  );
}

#[test]
fn to_hex_basic() {
  assert_eq!("00200aff".to_string(), [0x00, 0x20, 0x0a, 0xff].to_hex());
}
