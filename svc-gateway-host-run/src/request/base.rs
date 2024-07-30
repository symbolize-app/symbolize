use crate::header as svc_header;
use anyhow::Error;
use anyhow::Result;
use http::HeaderMap;
use http::HeaderValue;
use http::Request;
use hyper;
use hyper::body::Incoming as IncomingBody;

pub type BaseRequest = Request<IncomingBody>;

pub trait HeaderMapExt {
  fn get_key<'a, T>(&'a self) -> Option<T>
  where
    T: svc_header::HeaderPair + Sized + From<&'a HeaderValue>;

  fn try_get_key<'a, T>(&'a self) -> Result<Option<T>>
  where
    T: svc_header::HeaderPair
      + Sized
      + TryFrom<&'a HeaderValue, Error = Error>;
}

impl HeaderMapExt for HeaderMap<HeaderValue> {
  fn get_key<'a, T>(&'a self) -> Option<T>
  where
    T: svc_header::HeaderPair + Sized + From<&'a HeaderValue>,
  {
    self.get(T::key()).map(From::from)
  }

  fn try_get_key<'a, T>(&'a self) -> Result<Option<T>>
  where
    T: svc_header::HeaderPair
      + Sized
      + TryFrom<&'a HeaderValue, Error = Error>,
  {
    self.get(T::key()).map(TryFrom::try_from).transpose()
  }
}
