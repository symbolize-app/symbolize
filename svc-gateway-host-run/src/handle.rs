use crate::context as svc_context;
use crate::header as svc_header;
use crate::hex::FromHex as _;
use crate::path as svc_path;
use crate::response as svc_response_error;
use crate::response::BuilderExt as _;
use anyhow::Error;
use anyhow::Ok as AnyOk;
use anyhow::Result;
use bytes::Bytes;
use http::Method;
use http::StatusCode;
use http_body_util::Full as FullBody;
use hyper::body::Incoming as IncomingBody;
use hyper::Request;
use hyper::Response;
use rusqlite;
use tokio::task::spawn_blocking;

pub async fn handle(
  ctx: svc_context::Context,
  req: Request<IncomingBody>,
) -> Result<Response<FullBody<Bytes>>> {
  handle_paths(ctx, req).await.or_else(handle_error)
}

#[allow(clippy::print_stderr)]
fn handle_error(err: Error) -> Result<Response<FullBody<Bytes>>> {
  if err.is::<svc_response_error::Error>() {
    let err: svc_response_error::Error = err.downcast()?;
    Ok(
      Response::builder()
        .status(err.status)
        .full_body(err.message)?,
    )
  } else {
    eprintln!("Internal error {err:?}");
    Ok(
      Response::builder()
        .status(StatusCode::INTERNAL_SERVER_ERROR)
        .full_body("Internal error")?,
    )
  }
}

const CODE_ID_PATH: &str = "/.code/.id/";
const CODE_PATH: &str = "/.code/";
const INDEX_HTML_PATH: &str = "svc-gateway-guest-run/index.html";
const SERVICE_WORKER_SHELL_PATH: &str =
  "svc-gateway-guest-run/serviceWorkerShell.js";

async fn handle_paths(
  ctx: svc_context::Context,
  req: Request<IncomingBody>,
) -> Result<Response<FullBody<Bytes>>> {
  check_method(&req)?;

  let path = req.uri().path();
  let response = if let Some(path) = path.strip_prefix(CODE_ID_PATH) {
    handle_content_by_id(&ctx, &req, path).await?
  } else if let Some(path) = path.strip_prefix(CODE_PATH) {
    handle_content_by_path(&ctx, &req, path, true).await?
  } else if path == "/" {
    handle_content_by_path(&ctx, &req, INDEX_HTML_PATH, false).await?
  } else {
    None
  };
  if let Some(response) = response {
    Ok(response)
  } else {
    Ok(
      Response::builder()
        .status(StatusCode::NOT_FOUND)
        .full_body("Not found")?,
    )
  }
}

fn check_method(req: &Request<IncomingBody>) -> Result<()> {
  if req.method() == Method::GET {
    Ok(())
  } else {
    Err(
      svc_response_error::Error::new(
        StatusCode::BAD_REQUEST,
        "only GET supported",
      )
      .into(),
    )
  }
}

async fn handle_content_by_id(
  ctx: &svc_context::Context,
  req: &Request<IncomingBody>,
  path: &str,
) -> Result<Option<Response<FullBody<Bytes>>>> {
  let path = svc_path::parse_path(path, true)?;
  svc_path::check_path(req, &path)?;
  let db = ctx.db.clone();
  let content_id = Vec::from_hex(path.prefix)?;
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
  match content {
    None => Ok(None),
    Some(content) => Ok(Some(build_content_response(&path, content)?)),
  }
}

async fn handle_content_by_path(
  ctx: &svc_context::Context,
  req: &Request<IncomingBody>,
  path: &str,
  sandbox: bool,
) -> Result<Option<Response<FullBody<Bytes>>>> {
  let path = svc_path::parse_path(path, sandbox)?;
  svc_path::check_path(req, &path)?;
  let db = ctx.db.clone();
  let path_copy = path.full.to_owned();
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
  match content {
    None => Ok(None),
    Some(content) => Ok(Some(build_content_response(&path, content)?)),
  }
}

fn build_content_response(
  path: &svc_path::Path,
  content: Vec<u8>,
) -> Result<Response<FullBody<Bytes>>> {
  Ok(
    Response::builder()
      .header_pair(
        svc_header::ContentSecurityPolicy::builder()
          .default_source_self()
          .sandbox_opt(path.sandbox)
          .build(),
      )
      .header_pair(svc_header::ContentType(path.mime.into()))
      .header_pair_opt(
        (path.full == SERVICE_WORKER_SHELL_PATH)
          .then_some(svc_header::ServiceWorkerAllowed("/")),
      )
      .full_body(content)?,
  )
}
