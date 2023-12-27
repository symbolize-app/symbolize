use crate::header::pair::HeaderPair;
use http::header;
use http::header::InvalidHeaderValue;
use http::HeaderValue;
use hyper::header::HeaderName;

// https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/ETag
#[derive(Debug)]
pub struct ETag(pub String);

impl HeaderPair for ETag {
  fn key() -> &'static HeaderName {
    &header::ETAG
  }
}

impl TryFrom<ETag> for HeaderValue {
  type Error = InvalidHeaderValue;

  fn try_from(value: ETag) -> Result<Self, Self::Error> {
    let value = value.0;
    HeaderValue::try_from(format!("\"{value}\""))
  }
}
