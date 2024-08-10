use crate::header::pair::HeaderPair;
use anyhow::anyhow;
use anyhow::Error;
use http::header;
use http::HeaderValue;
use hyper::header::HeaderName;

// https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/If-None-Match
#[derive(Debug)]
pub struct IfNoneMatch(pub String);

impl HeaderPair for IfNoneMatch {
  fn key() -> &'static HeaderName {
    &header::IF_NONE_MATCH
  }
}

impl TryFrom<&HeaderValue> for IfNoneMatch {
  type Error = Error;

  fn try_from(value: &HeaderValue) -> Result<Self, Self::Error> {
    Ok(IfNoneMatch(
      value
        .to_str()?
        .strip_prefix('"')
        .ok_or(anyhow!("wrong prefix"))?
        .strip_suffix('"')
        .ok_or(anyhow!("wrong suffix"))?
        .to_owned(),
    ))
  }
}
