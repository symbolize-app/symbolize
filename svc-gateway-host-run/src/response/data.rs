use crate::header as svc_header;
use crate::response::base as svc_response_base;
use crate::response::base::BuilderExt as _;
use anyhow::Result;
use bytes::Bytes;
use http::StatusCode;
use hyper::Response;

pub struct DataResponse {
  pub status: StatusCode,
  pub sandbox: bool,
  pub cache_control: Option<svc_header::CacheControl>,
  pub content_type: svc_header::ContentType,
  pub e_tag: Option<svc_header::ETag>,
  pub service_worker_allowed: Option<svc_header::ServiceWorkerAllowed>,
  pub body: Bytes,
}

impl DataResponse {
  pub fn into_response(self) -> Result<svc_response_base::BaseResponse> {
    Ok(
      Response::builder()
        .status(self.status)
        .header_pair_opt(self.cache_control)
        .header_pair(
          svc_header::ContentSecurityPolicy::builder()
            .default_source_self()
            .image_source_self_data()
            .style_source_self_unsafe_inline()
            .sandbox_opt(self.sandbox)
            .build(),
        )
        .header_pair(self.content_type)
        .header_pair_opt(self.e_tag)
        .header_pair_opt(self.service_worker_allowed)
        .data_body(self.body)?,
    )
  }
}
