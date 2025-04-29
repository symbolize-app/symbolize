use crate::header as svc_header;
use anyhow::Error;
use anyhow::Result;
use http::HeaderMap;
use http::HeaderValue;
use http::Request;
use http::header::AsHeaderName;
use hyper;
use hyper::body::Incoming as IncomingBody;

pub type BaseRequest = Request<IncomingBody>;

trait HeaderMapStruct {
  fn get_<K>(&self, key: K) -> Option<&HeaderValue>
  where
    K: AsHeaderName;
}

impl HeaderMapStruct for HeaderMap<HeaderValue> {
  fn get_<K>(&self, key: K) -> Option<&HeaderValue>
  where
    K: AsHeaderName,
  {
    self.get(key)
  }
}

#[allow(private_bounds)]
pub trait HeaderMapExt: HeaderMapStruct {
  fn get_key<'a, T>(&'a self) -> Option<T>
  where
    T: svc_header::HeaderPair + Sized + From<&'a HeaderValue>,
  {
    self.get_(T::key()).map(From::from)
  }

  fn try_get_key<'a, T>(&'a self) -> Result<Option<T>>
  where
    T: svc_header::HeaderPair
      + Sized
      + TryFrom<&'a HeaderValue, Error = Error>,
  {
    self.get_(T::key()).map(TryFrom::try_from).transpose()
  }
}

impl HeaderMapExt for HeaderMap<HeaderValue> {}
