use crate::header::pair::HeaderPair;
use http::HeaderValue;
use hyper::header::HeaderName;

// https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Sec-Fetch-Dest
#[derive(Debug, Clone, Copy)]
pub enum SecFetchDest {
  Document,
  Empty,
  Script,
  ServiceWorker,
  Unknown,
}

static SEC_FETCH_DEST: HeaderName =
  HeaderName::from_static("sec-fetch-dest");

impl HeaderPair for SecFetchDest {
  fn key() -> &'static HeaderName {
    &SEC_FETCH_DEST
  }
}

impl From<&'_ HeaderValue> for SecFetchDest {
  fn from(value: &'_ HeaderValue) -> Self {
    if value == "document" {
      SecFetchDest::Document
    } else if value == "empty" {
      SecFetchDest::Empty
    } else if value == "script" {
      SecFetchDest::Script
    } else if value == "serviceworker" {
      SecFetchDest::ServiceWorker
    } else {
      SecFetchDest::Unknown
    }
  }
}
