use crate::context as svc_context;
use crate::db as svc_db;
use crate::db::Context as _;
use crate::hex::FromHex as _;
use crate::request as svc_request;
use crate::response as svc_response;
use anyhow::Error;
use anyhow::Result;
use bytes::Bytes;
use http::StatusCode;
use http_body_util::Full as FullBody;
use hyper::body::Incoming as IncomingBody;
use hyper::Request;
use hyper::Response;
use std::sync::Arc;

const CODE_ID_PREFIX: &str = "/.code/.id/";
const CODE_PREFIX: &str = "/.code/";
const INIT_HTML_PATH: &str = "svc-gateway-guest-run/init.html";

pub async fn handle<TContext>(
  ctx: Arc<TContext>,
  req: Request<IncomingBody>,
) -> Result<Response<FullBody<Bytes>>>
where
  TContext: svc_context::DbContext,
{
  handle_paths(ctx.as_ref(), &svc_request::SimpleRequest::new(&req))
    .await
    .unwrap_or_else(handle_error)
    .into_response()
}

#[allow(clippy::print_stderr)]
fn handle_error(err: Error) -> svc_response::SimpleResponse {
  let err = if err.is::<svc_response::Error>() {
    err.downcast().expect("downcast")
  } else {
    eprintln!("Internal error {err:?}");
    svc_response::Error::new(
      StatusCode::INTERNAL_SERVER_ERROR,
      "Internal error",
    )
  };
  err.into_simple_response()
}

async fn handle_paths<TContext>(
  ctx: &TContext,
  req: &svc_request::SimpleRequest<'_>,
) -> Result<svc_response::SimpleResponse>
where
  TContext: svc_context::DbContext,
{
  let path = req.path;
  let response = if let Some(path) = path.strip_prefix(CODE_ID_PREFIX) {
    handle_content_by_id(
      ctx,
      &svc_request::ContentRequest::try_new(req, path)?,
    )
    .await?
  } else if let Some(path) = path.strip_prefix(CODE_PREFIX) {
    handle_content_by_path(
      ctx,
      &svc_request::ContentRequest::try_new(req, path)?,
    )
    .await?
  } else if path == "/" {
    handle_content_by_path(
      ctx,
      &svc_request::ContentRequest::try_new_no_sandbox(
        req,
        INIT_HTML_PATH,
      )?,
    )
    .await?
  } else {
    None
  };
  if let Some(response) = response {
    Ok(response.into_simple_response())
  } else {
    Err(
      svc_response::Error::new(StatusCode::NOT_FOUND, "Not found").into(),
    )
  }
}

#[allow(clippy::print_stderr)]
async fn handle_content_by_id<TContext>(
  ctx: &TContext,
  req: &svc_request::ContentRequest<'_>,
) -> Result<Option<svc_response::ContentResponse>>
where
  TContext: svc_context::DbContext,
{
  let content_id = Vec::from_hex(req.path_prefix).map_err(|_| {
    eprintln!("  invalid content ID hex");
    svc_response::Error::new(
      StatusCode::BAD_REQUEST,
      "invalid content ID hex",
    )
  })?;
  let original = ctx.db().get_content_by_id(content_id.clone()).await?;
  if let Some(original) = original {
    Ok(Some(svc_response::ContentResponse::try_new_immutable(
      req,
      svc_db::ContentRowWithId {
        id: content_id,
        original,
      },
    )?))
  } else {
    Ok(None)
  }
}

async fn handle_content_by_path<TContext>(
  ctx: &TContext,
  req: &svc_request::ContentRequest<'_>,
) -> Result<Option<svc_response::ContentResponse>>
where
  TContext: svc_context::DbContext,
{
  let path_id = req.full_path.to_owned();
  let content_row = ctx.db().get_content_by_path(path_id).await?;
  if let Some(content_row) = content_row {
    Ok(Some(svc_response::ContentResponse::try_new(
      req,
      content_row,
    )?))
  } else {
    Ok(None)
  }
}

#[cfg(test)]
#[path = "./handle_test.rs"]
mod handle_test;
