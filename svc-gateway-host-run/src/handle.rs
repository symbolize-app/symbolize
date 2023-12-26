use crate::context as svc_context;
use crate::hex::FromHex as _;
use crate::request as svc_request;
use crate::response as svc_response;
use anyhow::Error;
use anyhow::Ok as AnyOk;
use anyhow::Result;
use bytes::Bytes;
use http::StatusCode;
use http_body_util::Full as FullBody;
use hyper::body::Incoming as IncomingBody;
use hyper::Request;
use hyper::Response;
use rusqlite;
use tokio::task::spawn_blocking;

const CODE_ID_PATH: &str = "/.code/.id/";
const CODE_PATH: &str = "/.code/";
const INDEX_HTML_PATH: &str = "svc-gateway-guest-run/index.html";

pub async fn handle(
  ctx: svc_context::Context,
  req: Request<IncomingBody>,
) -> Result<Response<FullBody<Bytes>>> {
  handle_paths(&ctx, &svc_request::SimpleRequest::new(&req))
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

async fn handle_paths(
  ctx: &svc_context::Context,
  req: &svc_request::SimpleRequest<'_>,
) -> Result<svc_response::SimpleResponse> {
  let path = req.path;
  let response = if let Some(path) = path.strip_prefix(CODE_ID_PATH) {
    handle_content_by_id(
      ctx,
      &svc_request::ContentRequest::try_new(req, path)?,
    )
    .await?
  } else if let Some(path) = path.strip_prefix(CODE_PATH) {
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
        INDEX_HTML_PATH,
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

async fn handle_content_by_id(
  ctx: &svc_context::Context,
  req: &svc_request::ContentRequest<'_>,
) -> Result<Option<svc_response::ContentResponse>> {
  let db = ctx.db.clone();
  let content_id = Vec::from_hex(req.path_prefix)?;
  let content = spawn_blocking(move || {
    let mut db = db.lock().expect("failed to lock db");
    let content: Option<Vec<u8>> = db.with_dependent_mut(|_, query| {
      query.get_content_by_id.query_row(
        rusqlite::named_params! {":content_id": content_id},
        |row| row.get(0),
      )
    })?;
    AnyOk(content)
  })
  .await??;
  Ok(
    content
      .map(|content| svc_response::ContentResponse::new(req, content)),
  )
}

async fn handle_content_by_path(
  ctx: &svc_context::Context,
  req: &svc_request::ContentRequest<'_>,
) -> Result<Option<svc_response::ContentResponse>> {
  let db = ctx.db.clone();
  let path_copy = req.full_path.to_owned();
  let content = spawn_blocking(move || {
    let mut db = db.lock().expect("failed to lock db");
    let content: Option<Vec<u8>> = db.with_dependent_mut(|_, query| {
      query.get_content_by_path.query_row(
        rusqlite::named_params! {":path_id": path_copy},
        |row| row.get(0),
      )
    })?;
    AnyOk(content)
  })
  .await??;
  Ok(
    content
      .map(|content| svc_response::ContentResponse::new(req, content)),
  )
}
