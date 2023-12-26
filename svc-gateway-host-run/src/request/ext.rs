use crate::header as svc_header;
use http::HeaderValue;
use http::Request;

pub trait RequestExt {
  fn header_pair<'a, T>(&'a self) -> Option<T>
  where
    T: svc_header::HeaderPair + Sized + TryFrom<&'a HeaderValue>;
}

impl<TBody> RequestExt for Request<TBody> {
  fn header_pair<'a, T>(&'a self) -> Option<T>
  where
    T: svc_header::HeaderPair + Sized + TryFrom<&'a HeaderValue>,
  {
    self
      .headers()
      .get(T::key())
      .and_then(|value| value.try_into().ok())
  }
}
