use super::_to_hex;
use super::*;

#[test]
fn from_hex_basic() {
  assert_eq!(
    vec![0x00, 0x20, 0x0a, 0xff],
    from_hex("\\x00200AFF").unwrap()
  );
}

#[test]
fn to_hex_basic() {
  assert_eq!(
    "\\x00200aff".to_string(),
    _to_hex(&[0x00, 0x20, 0x0a, 0xff])
  );
}
