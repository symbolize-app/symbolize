use crate::header as svc_header;
use crate::request::ext::RequestExt as _;
use http::Method;
use hyper::body::Incoming as IncomingBody;
use hyper::Request;

pub struct SimpleRequest<'a> {
  pub method: &'a Method,
  pub path: &'a str,
  pub accept: Option<svc_header::Accept<'a>>,
  pub if_none_match: Option<svc_header::IfNoneMatch<'a>>,
  pub sec_fetch_dest: Option<svc_header::SecFetchDest>,
  pub sec_fetch_mode: Option<svc_header::SecFetchMode>,
  pub sec_fetch_site: Option<svc_header::SecFetchSite>,
}

impl SimpleRequest<'_> {
  pub fn new(req: &Request<IncomingBody>) -> SimpleRequest<'_> {
    SimpleRequest {
      method: req.method(),
      path: req.uri().path(),
      accept: req.header_pair(),
      if_none_match: req.header_pair(),
      sec_fetch_dest: req.header_pair(),
      sec_fetch_mode: req.header_pair(),
      sec_fetch_site: req.header_pair(),
    }
  }
}
