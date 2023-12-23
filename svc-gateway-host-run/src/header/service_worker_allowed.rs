use crate::header::pair::HeaderPair;
use http::header::InvalidHeaderValue;
use http::HeaderValue;
use hyper::header::HeaderName;

// https://www.w3.org/TR/service-workers/#service-worker-allowed
#[derive(Debug)]
pub struct ServiceWorkerAllowed<'a>(pub &'a str);

static SERVICE_WORKER_ALLOWED: HeaderName =
  HeaderName::from_static("service-worker-allowed");

impl HeaderPair for ServiceWorkerAllowed<'_> {
  fn key() -> &'static HeaderName {
    &SERVICE_WORKER_ALLOWED
  }
}

impl<'a> TryFrom<ServiceWorkerAllowed<'a>> for HeaderValue {
  type Error = InvalidHeaderValue;

  fn try_from(
    value: ServiceWorkerAllowed<'a>,
  ) -> Result<Self, Self::Error> {
    HeaderValue::try_from(value.0)
  }
}
