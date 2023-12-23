use crate::header as svc_header;
use crate::request::RequestExt as _;
use crate::response as svc_response_error;
use anyhow::anyhow;
use anyhow::Result;
use http::StatusCode;
use hyper::body::Incoming as IncomingBody;
use hyper::Request;
use mime;

#[derive(Debug)]
pub struct Path<'a> {
  pub full: &'a str,
  pub prefix: &'a str,
  pub mime: ContentMime,
  pub sandbox: bool,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ContentMime {
  Html,
  JavaScript,
}

impl From<ContentMime> for &'static mime::Mime {
  fn from(value: ContentMime) -> Self {
    match value {
      ContentMime::Html => &mime::TEXT_HTML,
      ContentMime::JavaScript => &mime::TEXT_JAVASCRIPT,
    }
  }
}

pub fn parse_path(full: &str, sandbox: bool) -> Result<Path> {
  let (prefix, ext) =
    full.rsplit_once('.').ok_or(anyhow!("invalid hex path"))?;
  let mime = match ext {
    "html" => Ok(ContentMime::Html),
    "js" | "mjs" => Ok(ContentMime::JavaScript),
    _ => Err(anyhow!("invalid extension for {full:?}")),
  }?;
  let sandbox = sandbox && matches!(mime, ContentMime::Html);
  Ok(Path {
    full,
    prefix,
    mime,
    sandbox,
  })
}

#[allow(clippy::print_stdout)]
pub fn check_path(req: &Request<IncomingBody>, path: &Path) -> Result<()> {
  let full_path = path.full;
  println!("{full_path}");
  if let Some(accept) = req.header_pair() {
    check_accept(accept, path.mime)?;
  }
  if let (Some(dest), Some(mode), Some(site)) =
    (req.header_pair(), req.header_pair(), req.header_pair())
  {
    check_fetch_metadata(dest, mode, site, path.mime)?;
  }
  Ok(())
}

#[allow(clippy::print_stderr)]
fn check_accept(
  mut accept: svc_header::Accept,
  mime: ContentMime,
) -> Result<()> {
  let mime: &mime::Mime = mime.into();
  let found = accept.0.any(|item| {
    item
      .map(|item| match (item.type_(), item.subtype()) {
        (mime::STAR, mime::STAR) => true,
        (mime::STAR, subtype) => subtype == mime.subtype(),
        (type_, subtype) => {
          (type_, subtype) == (mime.type_(), mime.subtype())
        }
      })
      .unwrap_or(false)
  });
  if found {
    Ok(())
  } else {
    eprint!("wrong accept type {accept:?} {mime:?}");
    Err(
      svc_response_error::Error::new(
        StatusCode::BAD_REQUEST,
        "wrong accept type",
      )
      .into(),
    )
  }
}

#[allow(clippy::print_stderr)]
fn check_fetch_metadata(
  dest: svc_header::SecFetchDest,
  mode: svc_header::SecFetchMode,
  site: svc_header::SecFetchSite,
  mime: ContentMime,
) -> Result<()> {
  let safe = matches!(
    (dest, mode, site, mime),
    (
      svc_header::SecFetchDest::Document,
      svc_header::SecFetchMode::Navigate,
      svc_header::SecFetchSite::None,
      ContentMime::Html,
    ) | (
      svc_header::SecFetchDest::Script
        | svc_header::SecFetchDest::ServiceWorker,
      _,
      svc_header::SecFetchSite::SameOrigin,
      ContentMime::JavaScript,
    ) | (
      svc_header::SecFetchDest::Empty,
      svc_header::SecFetchMode::Cors,
      svc_header::SecFetchSite::SameOrigin,
      _,
    )
  );
  if safe {
    Ok(())
  } else {
    eprintln!(
      "  wrong fetch metadata {dest:?} {mode:?} {site:?} {mime:?}"
    );
    Err(
      svc_response_error::Error::new(
        StatusCode::BAD_REQUEST,
        "wrong fetch metadata",
      )
      .into(),
    )
  }
}
