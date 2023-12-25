use crate::header::pair::HeaderPair;
use http::header;
use http::header::InvalidHeaderValue;
use http::HeaderValue;
use hyper::header::HeaderName;
use mime;

// https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Content-Type
#[derive(Debug)]
pub struct ContentType(pub &'static mime::Mime);

impl HeaderPair for ContentType {
  fn key() -> &'static HeaderName {
    &header::CONTENT_TYPE
  }
}

impl TryFrom<ContentType> for HeaderValue {
  type Error = InvalidHeaderValue;

  fn try_from(value: ContentType) -> Result<Self, Self::Error> {
    HeaderValue::try_from(value.0.as_ref())
  }
}
