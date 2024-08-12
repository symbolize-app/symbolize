use crate::header::pair::HeaderPair;
use http::header;
use http::header::InvalidHeaderValue;
use http::HeaderValue;
use hyper::header::HeaderName;

// https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Cache-Control
#[derive(Debug)]
pub struct CacheControl(pub Vec<Directive>);

impl CacheControl {
  #[must_use]
  pub fn builder() -> Builder {
    Builder(Vec::new())
  }
}

#[derive(Debug, Clone, Copy)]
pub enum Directive {
  MaxAge30Days,
  Immutable,
  NoCache,
}

impl Directive {
  fn to_str(self) -> &'static str {
    match self {
      Self::MaxAge30Days => "max-age=2592000",
      Self::Immutable => "immutable",
      Self::NoCache => "no-cache",
    }
  }
}

#[derive(Debug)]
pub struct Builder(Vec<Directive>);

impl Builder {
  pub fn max_age_30_days(mut self) -> Self {
    self.0.push(Directive::MaxAge30Days);
    self
  }

  pub fn immutable_opt(mut self, enable: bool) -> Self {
    if enable {
      self.0.push(Directive::Immutable);
    }
    self
  }

  pub fn no_cache_opt(mut self, enable: bool) -> Self {
    if enable {
      self.0.push(Directive::NoCache);
    }
    self
  }

  pub fn build(self) -> CacheControl {
    CacheControl(self.0)
  }
}

impl HeaderPair for CacheControl {
  fn key() -> &'static HeaderName {
    &header::CACHE_CONTROL
  }
}

impl TryFrom<CacheControl> for HeaderValue {
  type Error = InvalidHeaderValue;

  fn try_from(value: CacheControl) -> Result<Self, Self::Error> {
    HeaderValue::try_from(
      value
        .0
        .iter()
        .map(|item| item.to_str())
        .collect::<Vec<&'static str>>()
        .join(","),
    )
  }
}
