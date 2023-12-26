use crate::header as svc_header;
use crate::request::ext::RequestExt as _;
use http::Method;
use hyper::body::Incoming as IncomingBody;
use hyper::Request;

pub struct SimpleRequest<'a> {
  pub method: &'a Method,
  pub path: &'a str,
  pub accept: Option<svc_header::Accept<'a>>,
  pub dest: Option<svc_header::SecFetchDest>,
  pub mode: Option<svc_header::SecFetchMode>,
  pub site: Option<svc_header::SecFetchSite>,
}

impl SimpleRequest<'_> {
  pub fn new(req: &Request<IncomingBody>) -> SimpleRequest<'_> {
    SimpleRequest {
      method: req.method(),
      path: req.uri().path(),
      accept: req.header_pair(),
      dest: req.header_pair(),
      mode: req.header_pair(),
      site: req.header_pair(),
    }
  }
}
