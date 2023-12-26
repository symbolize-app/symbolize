use crate::header as svc_header;
use crate::request as svc_request;
use crate::response::simple as svc_response_simple;
use bytes::Bytes;
use http::StatusCode;

const SERVICE_WORKER_SHELL_PATH: &str =
  "svc-gateway-guest-run/serviceWorkerShell.js";

#[derive(Debug, PartialEq, Eq)]
pub struct ContentResponse {
  pub sandbox: bool,
  pub mime: svc_request::ContentMime,
  pub is_service_worker_shell: bool,
  pub body: Bytes,
}

impl ContentResponse {
  pub fn map_new<T>(
    req: &svc_request::ContentRequest,
    body: Option<T>,
  ) -> Option<Self>
  where
    Bytes: From<T>,
  {
    body.map(|body| Self {
      sandbox: req.sandbox,
      mime: req.mime,
      is_service_worker_shell: req.full_path == SERVICE_WORKER_SHELL_PATH,
      body: body.into(),
    })
  }

  pub fn into_simple_response(
    self,
  ) -> svc_response_simple::SimpleResponse {
    svc_response_simple::SimpleResponse {
      status: StatusCode::OK,
      content_security_policy: svc_header::ContentSecurityPolicy::builder(
      )
      .default_source_self()
      .sandbox_opt(self.sandbox)
      .build(),
      content_type: svc_header::ContentType(self.mime.into()),
      service_worker_allowed: self
        .is_service_worker_shell
        .then_some(svc_header::ServiceWorkerAllowed("/")),
      body: self.body,
    }
  }
}
