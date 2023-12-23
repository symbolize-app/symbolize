use hyper::header::HeaderName;

pub trait HeaderPair {
  fn key() -> &'static HeaderName;
}
