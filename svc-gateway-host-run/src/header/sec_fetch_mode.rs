use crate::header::pair::HeaderPair;
use http::HeaderValue;
use hyper::header::HeaderName;

// https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Sec-Fetch-Mode
#[derive(Debug, Clone, Copy)]
pub enum SecFetchMode {
  Cors,
  Navigate,
  Unknown,
}

static SEC_FETCH_MODE: HeaderName =
  HeaderName::from_static("sec-fetch-mode");

impl HeaderPair for SecFetchMode {
  fn key() -> &'static HeaderName {
    &SEC_FETCH_MODE
  }
}

impl From<&'_ HeaderValue> for SecFetchMode {
  fn from(value: &'_ HeaderValue) -> Self {
    if value == "cors" {
      SecFetchMode::Cors
    } else if value == "navigate" {
      SecFetchMode::Navigate
    } else {
      SecFetchMode::Unknown
    }
  }
}
