use crate::header as svc_header;
use crate::response::ext::BuilderExt as _;
use anyhow::Result;
use bytes::Bytes;
use http::StatusCode;
use http_body_util::Full as FullBody;
use hyper::Response;

pub struct SimpleResponse {
  pub status: StatusCode,
  pub content_security_policy: svc_header::ContentSecurityPolicy,
  pub content_type: svc_header::ContentType,
  pub service_worker_allowed: Option<svc_header::ServiceWorkerAllowed>,
  pub body: Bytes,
}

impl SimpleResponse {
  pub fn into_response(self) -> Result<Response<FullBody<Bytes>>> {
    Ok(
      Response::builder()
        .status(self.status)
        .header_pair(self.content_security_policy)
        .header_pair(self.content_type)
        .header_pair_opt(self.service_worker_allowed)
        .full_body(self.body)?,
    )
  }
}
