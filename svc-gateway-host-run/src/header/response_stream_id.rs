use crate::header::pair::HeaderPair;
use http::header::InvalidHeaderValue;
use http::HeaderValue;
use hyper::header::HeaderName;
use intertwine_lib_hex::ToHex as _;

pub struct ResponseStreamId(pub [u8; 256]);

static RESPONSE_STREAM_ID: HeaderName =
  HeaderName::from_static("response-stream-id");

impl HeaderPair for ResponseStreamId {
  fn key() -> &'static HeaderName {
    &RESPONSE_STREAM_ID
  }
}

impl TryFrom<ResponseStreamId> for HeaderValue {
  type Error = InvalidHeaderValue;

  fn try_from(value: ResponseStreamId) -> Result<Self, Self::Error> {
    let value = value.0;
    HeaderValue::try_from(value.to_hex())
  }
}
