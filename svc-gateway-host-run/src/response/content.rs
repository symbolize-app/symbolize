use crate::db as svc_db;
use crate::header as svc_header;
use crate::request as svc_request;
use crate::response::error as svc_response_error;
use crate::response::simple as svc_response_simple;
use anyhow::Result;
use http::StatusCode;
use intertwine_lib_hex::ToHex as _;

const SERVICE_WORKER_SHELL_PATH: &str =
  "svc-gateway-guest-run/serviceWorkerShell.js";

#[derive(Debug, PartialEq, Eq)]
pub struct ContentResponse {
  pub mime: svc_request::ContentMime,
  pub sandbox: bool,
  pub is_service_worker_shell: bool,
  pub immutable: bool,
  pub e_tag: String,
  pub original: Vec<u8>,
}

impl ContentResponse {
  pub fn try_new_immutable(
    req: &svc_request::ContentRequest,
    content_row: svc_db::ContentRowWithId,
  ) -> Result<Self> {
    let immutable = true;
    Self::try_new_base(req, content_row, immutable)
  }

  pub fn try_new(
    req: &svc_request::ContentRequest,
    content_row: svc_db::ContentRowWithId,
  ) -> Result<Self> {
    let immutable = false;
    Self::try_new_base(req, content_row, immutable)
  }

  #[allow(clippy::print_stderr)]
  fn try_new_base(
    req: &svc_request::ContentRequest,
    content_row: svc_db::ContentRowWithId,
    immutable: bool,
  ) -> Result<Self> {
    let e_tag = content_row.id.to_hex();
    if Some(e_tag.as_ref()) == req.if_none_match {
      eprintln!("  not modified");
      Err(
        svc_response_error::Error::new(StatusCode::NOT_MODIFIED, "N/A")
          .into(),
      )
    } else {
      Ok(Self {
        sandbox: req.sandbox,
        mime: req.mime,
        is_service_worker_shell: !immutable
          && req.full_path == SERVICE_WORKER_SHELL_PATH,
        immutable,
        e_tag,
        original: content_row.original,
      })
    }
  }

  pub fn into_simple_response(
    self,
  ) -> svc_response_simple::SimpleResponse {
    svc_response_simple::SimpleResponse {
      status: StatusCode::OK,
      sandbox: self.sandbox,
      cache_control: Some(
        svc_header::CacheControl::builder()
          .max_age_30_days()
          .immutable_opt(self.immutable)
          .no_cache_opt(!self.immutable)
          .build(),
      ),
      content_type: svc_header::ContentType(self.mime.into()),
      e_tag: Some(svc_header::ETag(self.e_tag)),
      service_worker_allowed: self
        .is_service_worker_shell
        .then_some(svc_header::ServiceWorkerAllowed("/")),
      body: self.original.into(),
    }
  }
}

#[cfg(test)]
#[path = "./content_test.rs"]
mod content_test;
