use crate::header as svc_header;
use bytes::Bytes;
use http;
use http::response::Builder;
use http::HeaderValue;
use http::Response;
use http_body_util::Full as FullBody;

pub trait BuilderExt {
  fn header_pair<T>(self, header: T) -> Self
  where
    T: svc_header::HeaderPair,
    HeaderValue: TryFrom<T>,
    <HeaderValue as TryFrom<T>>::Error: Into<http::Error>;

  fn header_pair_opt<T>(self, header: Option<T>) -> Self
  where
    T: svc_header::HeaderPair,
    HeaderValue: TryFrom<T>,
    <HeaderValue as TryFrom<T>>::Error: Into<http::Error>;

  fn full_body<T>(
    self,
    body: T,
  ) -> http::Result<Response<FullBody<Bytes>>>
  where
    Bytes: From<T>;
}

impl BuilderExt for Builder {
  fn header_pair<T>(self, header: T) -> Self
  where
    T: svc_header::HeaderPair,
    HeaderValue: TryFrom<T>,
    <HeaderValue as TryFrom<T>>::Error: Into<http::Error>,
  {
    self.header(T::key(), header)
  }

  fn header_pair_opt<T>(self, header: Option<T>) -> Self
  where
    T: svc_header::HeaderPair,
    HeaderValue: TryFrom<T>,
    <HeaderValue as TryFrom<T>>::Error: Into<http::Error>,
  {
    if let Some(header) = header {
      self.header(T::key(), header)
    } else {
      self
    }
  }

  fn full_body<T>(self, body: T) -> http::Result<Response<FullBody<Bytes>>>
  where
    Bytes: From<T>,
  {
    self.body(FullBody::new(Bytes::from(body)))
  }
}
