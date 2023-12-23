use crate::header::pair::HeaderPair;
use http::HeaderValue;
use hyper::header::HeaderName;

// https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Sec-Fetch-Site
#[derive(Debug, Clone, Copy)]
pub enum SecFetchSite {
  None,
  SameOrigin,
  Unknown,
}

static SEC_FETCH_SITE: HeaderName =
  HeaderName::from_static("sec-fetch-site");

impl HeaderPair for SecFetchSite {
  fn key() -> &'static HeaderName {
    &SEC_FETCH_SITE
  }
}

impl From<&'_ HeaderValue> for SecFetchSite {
  fn from(value: &'_ HeaderValue) -> Self {
    if value == "none" {
      SecFetchSite::None
    } else if value == "same-origin" {
      SecFetchSite::SameOrigin
    } else {
      SecFetchSite::Unknown
    }
  }
}
