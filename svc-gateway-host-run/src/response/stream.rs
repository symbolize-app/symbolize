use crate::header as svc_header;
use crate::response::base as svc_response_base;
use crate::response::base::BuilderExt as _;
use anyhow::Result;
use bytes::Bytes;
use futures::stream;
use http::StatusCode;
use hyper::Response;

pub struct StreamResponse<S> {
  pub response_stream_id: Option<[u8; 256]>,
  pub body: S,
}

impl<S> StreamResponse<S> {
  pub fn into_response<T>(self) -> Result<svc_response_base::BaseResponse>
  where
    S: stream::Stream<Item = Result<T>> + Sync + Send + 'static,
    Bytes: From<T>,
  {
    Ok(
      Response::builder()
        .status(StatusCode::OK)
        .header_pair_opt(
          self.response_stream_id.map(svc_header::ResponseStreamId),
        )
        .header_pair(
          svc_header::ContentSecurityPolicy::builder()
            .default_source_self()
            .image_source_self_data()
            .style_source_self_unsafe_inline()
            .sandbox_opt(true)
            .build(),
        )
        .stream_body(self.body)?,
    )
  }
}
