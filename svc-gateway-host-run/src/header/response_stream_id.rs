use crate::header::pair::HeaderPair;
use http::HeaderValue;
use http::header::InvalidHeaderValue;
use hyper::header::HeaderName;
use symbolize_lib_hex::ToHex as _;

#[derive(Clone, Debug)]
pub struct ResponseStreamId(pub [u8; 256]);

impl Copy for ResponseStreamId {}

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
