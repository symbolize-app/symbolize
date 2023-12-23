use crate::header::pair::HeaderPair;
use http::header;
use http::header::ToStrError;
use http::HeaderValue;
use hyper::header::HeaderName;
use mime;

// https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Accept
#[derive(Debug)]
pub struct Accept<'a>(pub mime::MimeIter<'a>);

impl HeaderPair for Accept<'_> {
  fn key() -> &'static HeaderName {
    &header::ACCEPT
  }
}

impl<'a> TryFrom<&'a HeaderValue> for Accept<'a> {
  type Error = ToStrError;

  fn try_from(value: &'a HeaderValue) -> Result<Self, Self::Error> {
    Ok(Accept(mime::MimeIter::new(value.to_str()?)))
  }
}
