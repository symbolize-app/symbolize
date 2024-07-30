use crate::header as svc_header;
use crate::request::data as svc_request_data;
use crate::response as svc_response;
use anyhow::anyhow;
use anyhow::Result;
use http::Method;
use http::StatusCode;
use mime;

#[derive(Debug)]
pub struct ContentRequest {
  pub full_path: String,
  pub path_prefix: String,
  pub mime: ContentMime,
  pub sandbox: bool,
  pub if_none_match: Option<String>,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ContentMime {
  Html,
  JavaScript,
  Woff2,
}

impl ContentRequest {
  pub fn try_new_no_sandbox(
    req: svc_request_data::DataRequest,
    full_path: String,
  ) -> Result<ContentRequest> {
    Self::try_new_base(req, full_path, false)
  }

  pub fn try_new(
    req: svc_request_data::DataRequest,
    full_path: String,
  ) -> Result<ContentRequest> {
    Self::try_new_base(req, full_path, true)
  }

  #[allow(clippy::print_stdout)]
  fn try_new_base(
    req: svc_request_data::DataRequest,
    full_path: String,
    sandbox: bool,
  ) -> Result<ContentRequest> {
    println!("{}", req.path);

    let (path_prefix, ext) = Self::split_path(&full_path)?;
    let path_prefix = path_prefix.to_owned();
    let mime = Self::get_mime(&full_path, ext)?;
    let sandbox = sandbox && matches!(mime, ContentMime::Html);
    let if_none_match =
      req.if_none_match.map(|if_none_match| if_none_match.0);

    Self::check_method(&req.method)?;
    if let Some(accept) = &req.accept {
      Self::check_accept(accept, mime)?;
    }
    if let (Some(dest), Some(mode), Some(site)) =
      (req.sec_fetch_dest, req.sec_fetch_mode, req.sec_fetch_site)
    {
      Self::check_fetch_metadata(dest, mode, site, mime)?;
    }

    Ok(ContentRequest {
      full_path,
      path_prefix,
      mime,
      sandbox,
      if_none_match,
    })
  }

  fn split_path(full_path: &str) -> Result<(&str, &str)> {
    full_path
      .rsplit_once('.')
      .ok_or(anyhow!("invalid hex path"))
  }

  fn get_mime(full_path: &str, ext: &str) -> Result<ContentMime> {
    match ext {
      "html" => Ok(ContentMime::Html),
      "js" | "mjs" => Ok(ContentMime::JavaScript),
      "woff2" => Ok(ContentMime::Woff2),
      _ => Err(anyhow!("invalid extension for {full_path:?}")),
    }
  }

  fn check_method(method: &Method) -> Result<()> {
    if method == Method::GET {
      Ok(())
    } else {
      Err(
        svc_response::Error::new(
          StatusCode::BAD_REQUEST,
          "only GET supported",
        )
        .into(),
      )
    }
  }

  #[allow(clippy::print_stderr)]
  fn check_accept(
    accept: &svc_header::Accept,
    mime: ContentMime,
  ) -> Result<()> {
    let mime: &mime::Mime = mime.into();
    let found =
      accept
        .0
        .iter()
        .any(|item| match (item.type_(), item.subtype()) {
          (mime::STAR, mime::STAR) => true,
          (mime::STAR, subtype) => subtype == mime.subtype(),
          (type_, subtype) => {
            (type_, subtype) == (mime.type_(), mime.subtype())
          }
        });
    if found {
      Ok(())
    } else {
      eprintln!("  wrong accept type {accept:?} {mime:?}");
      Err(
        svc_response::Error::new(
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
        _,
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
        svc_response::Error::new(
          StatusCode::BAD_REQUEST,
          "wrong fetch metadata",
        )
        .into(),
      )
    }
  }
}

impl From<ContentMime> for &'static mime::Mime {
  fn from(value: ContentMime) -> Self {
    match value {
      ContentMime::Html => &mime::TEXT_HTML,
      ContentMime::JavaScript => &mime::TEXT_JAVASCRIPT,
      ContentMime::Woff2 => &mime::FONT_WOFF2,
    }
  }
}
