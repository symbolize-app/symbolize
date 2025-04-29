use crate::header::pair::HeaderPair;
use http::HeaderValue;
use http::header;
use http::header::InvalidHeaderValue;
use hyper::header::HeaderName;

// https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Content-Type
#[derive(Debug)]
pub struct ContentSecurityPolicy(pub Vec<Directive>);

impl ContentSecurityPolicy {
  #[must_use]
  pub fn builder() -> Builder {
    Builder(Vec::new())
  }
}

#[derive(Debug, Clone, Copy)]
pub enum Directive {
  DefaultSourceSelf,
  ImageSourceSelfData,
  StyleSourceSelfUnsafeInline,
  Sandbox,
}

impl Directive {
  fn to_str(self) -> &'static str {
    match self {
      Self::DefaultSourceSelf => "default-src 'self'",
      Self::ImageSourceSelfData => "img-src 'self' data:",
      Self::StyleSourceSelfUnsafeInline => {
        "style-src 'self' 'unsafe-inline'"
      }
      Self::Sandbox => "sandbox",
    }
  }
}

#[derive(Debug)]
pub struct Builder(Vec<Directive>);

impl Builder {
  pub fn default_source_self(mut self) -> Self {
    self.0.push(Directive::DefaultSourceSelf);
    self
  }

  pub fn image_source_self_data(mut self) -> Self {
    self.0.push(Directive::ImageSourceSelfData);
    self
  }

  pub fn style_source_self_unsafe_inline(mut self) -> Self {
    self.0.push(Directive::StyleSourceSelfUnsafeInline);
    self
  }

  pub fn sandbox_opt(mut self, enable: bool) -> Self {
    if enable {
      self.0.push(Directive::Sandbox);
    }
    self
  }

  pub fn build(self) -> ContentSecurityPolicy {
    ContentSecurityPolicy(self.0)
  }
}

impl HeaderPair for ContentSecurityPolicy {
  fn key() -> &'static HeaderName {
    &header::CONTENT_SECURITY_POLICY
  }
}

impl TryFrom<ContentSecurityPolicy> for HeaderValue {
  type Error = InvalidHeaderValue;

  fn try_from(value: ContentSecurityPolicy) -> Result<Self, Self::Error> {
    HeaderValue::try_from(
      value
        .0
        .iter()
        .map(|item| item.to_str())
        .collect::<Vec<&'static str>>()
        .join(";"),
    )
  }
}
