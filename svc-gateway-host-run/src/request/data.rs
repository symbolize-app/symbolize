use crate::header as svc_header;
use crate::request::base as svc_request_base;
use crate::request::base::HeaderMapExt as _;
use anyhow::Result;
use http::Method;

pub struct DataRequest {
  pub method: Method,
  pub path: String,
  pub accept: Option<svc_header::Accept>,
  pub if_none_match: Option<svc_header::IfNoneMatch>,
  pub sec_fetch_dest: Option<svc_header::SecFetchDest>,
  pub sec_fetch_mode: Option<svc_header::SecFetchMode>,
  pub sec_fetch_site: Option<svc_header::SecFetchSite>,
}

impl DataRequest {
  pub fn new(req: svc_request_base::BaseRequest) -> Result<DataRequest> {
    let (parts, _body) = req.into_parts();
    let result = DataRequest {
      method: parts.method,
      path: parts.uri.path().to_owned(),
      accept: parts.headers.try_get_key()?,
      if_none_match: parts.headers.try_get_key()?,
      sec_fetch_dest: parts.headers.get_key(),
      sec_fetch_mode: parts.headers.get_key(),
      sec_fetch_site: parts.headers.get_key(),
    };
    Ok(result)
  }
}
