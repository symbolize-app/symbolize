use crate::header as svc_header;
use crate::response::base as svc_response_base;
use anyhow::Error;
use anyhow::Result;
use bytes::Bytes;
use futures::stream;
use futures::stream::StreamExt as _;
use http;
use http::response::Builder;
use http::HeaderName;
use http::HeaderValue;
use http_body::Body;
use http_body::Frame;
use http_body_util::BodyExt as _;
use http_body_util::Full as FullBody;
use http_body_util::StreamBody;
use hyper::Response;
use std::pin::Pin;

pub type BaseResponse =
  Response<Pin<Box<dyn Body<Data = Bytes, Error = Error> + Sync + Send>>>;

trait BuilderStruct: Sized {
  fn header_<K, V>(self, key: K, value: V) -> Self
  where
    HeaderName: TryFrom<K>,
    <HeaderName as TryFrom<K>>::Error: Into<http::Error>,
    HeaderValue: TryFrom<V>,
    <HeaderValue as TryFrom<V>>::Error: Into<http::Error>;

  fn body_<T>(self, body: T) -> http::Result<Response<T>>;
}

impl BuilderStruct for Builder {
  fn header_<K, V>(self, key: K, value: V) -> Self
  where
    HeaderName: TryFrom<K>,
    <HeaderName as TryFrom<K>>::Error: Into<http::Error>,
    HeaderValue: TryFrom<V>,
    <HeaderValue as TryFrom<V>>::Error: Into<http::Error>,
  {
    self.header(key, value)
  }

  fn body_<T>(self, body: T) -> http::Result<Response<T>> {
    self.body(body)
  }
}

#[allow(private_bounds)]
pub trait BuilderExt: BuilderStruct {
  fn header_pair<T>(self, header: T) -> Self
  where
    T: svc_header::HeaderPair,
    HeaderValue: TryFrom<T>,
    <HeaderValue as TryFrom<T>>::Error: Into<http::Error>,
  {
    self.header_(T::key(), header)
  }

  fn header_pair_opt<T>(self, header: Option<T>) -> Self
  where
    T: svc_header::HeaderPair,
    HeaderValue: TryFrom<T>,
    <HeaderValue as TryFrom<T>>::Error: Into<http::Error>,
  {
    if let Some(header) = header {
      self.header_(T::key(), header)
    } else {
      self
    }
  }

  fn data_body<T>(
    self,
    body: T,
  ) -> http::Result<svc_response_base::BaseResponse>
  where
    Bytes: From<T>,
  {
    self.body_(Box::pin(
      FullBody::new(Bytes::from(body)).map_err(anyhow::Error::new),
    ))
  }

  fn stream_body<S, T>(
    self,
    body: S,
  ) -> http::Result<svc_response_base::BaseResponse>
  where
    S: stream::Stream<Item = Result<T>> + Sync + Send + 'static,
    Bytes: From<T>,
  {
    let body =
      body.map(|item| item.map(|item| Frame::data(Bytes::from(item))));
    self.body_(Box::pin(StreamBody::new(body)))
  }
}

impl BuilderExt for Builder {}
