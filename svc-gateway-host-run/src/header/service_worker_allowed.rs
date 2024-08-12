use crate::header::pair::HeaderPair;
use http::header::InvalidHeaderValue;
use http::HeaderValue;
use hyper::header::HeaderName;

// https://www.w3.org/TR/service-workers/#service-worker-allowed
#[derive(Clone, Debug)]
pub struct ServiceWorkerAllowed(pub &'static str);

impl Copy for ServiceWorkerAllowed {}

static SERVICE_WORKER_ALLOWED: HeaderName =
  HeaderName::from_static("service-worker-allowed");

impl HeaderPair for ServiceWorkerAllowed {
  fn key() -> &'static HeaderName {
    &SERVICE_WORKER_ALLOWED
  }
}

impl TryFrom<ServiceWorkerAllowed> for HeaderValue {
  type Error = InvalidHeaderValue;

  fn try_from(value: ServiceWorkerAllowed) -> Result<Self, Self::Error> {
    HeaderValue::try_from(value.0)
  }
}
