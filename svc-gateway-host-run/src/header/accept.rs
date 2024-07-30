use crate::header::pair::HeaderPair;
use anyhow::Error;
use http::header;
use http::HeaderValue;
use hyper::header::HeaderName;
use mime;

// https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Accept
#[derive(Debug, Clone)]
pub struct Accept(pub Vec<mime::Mime>);

impl HeaderPair for Accept {
  fn key() -> &'static HeaderName {
    &header::ACCEPT
  }
}

impl TryFrom<&HeaderValue> for Accept {
  type Error = Error;

  fn try_from(value: &HeaderValue) -> Result<Self, Self::Error> {
    Ok(Accept(
      mime::MimeIter::new(value.to_str()?)
        .collect::<Result<Vec<_>, _>>()
        .map_err(|err| Error::msg(err.to_owned()))?,
    ))
  }
}
