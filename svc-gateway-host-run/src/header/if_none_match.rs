use crate::header::pair::HeaderPair;
use anyhow::anyhow;
use anyhow::Error as AnyError;
use http::header;
use http::HeaderValue;
use hyper::header::HeaderName;

// https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/If-None-Match
#[derive(Debug)]
pub struct IfNoneMatch<'a>(pub &'a str);

impl HeaderPair for IfNoneMatch<'_> {
  fn key() -> &'static HeaderName {
    &header::IF_NONE_MATCH
  }
}

impl<'a> TryFrom<&'a HeaderValue> for IfNoneMatch<'a> {
  type Error = AnyError;

  fn try_from(value: &'a HeaderValue) -> Result<Self, Self::Error> {
    Ok(IfNoneMatch(
      value
        .to_str()?
        .strip_prefix('"')
        .ok_or(anyhow!("wrong prefix"))?
        .strip_suffix('"')
        .ok_or(anyhow!("wrong suffix"))?,
    ))
  }
}
