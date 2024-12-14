use crate::request::base as svc_request_base;
use anyhow::Result;
use form_urlencoded;
use http::Method;
use http_body_util::BodyDataStream;
use http_body_util::BodyExt as _;
use hyper::body::Incoming as IncomingBody;
use symbolize_lib_hex::FromHex as _;

#[derive(Debug)]
pub struct StreamRequest<S> {
  pub method: Method,
  pub response_stream_id: Option<Vec<u8>>,
  pub data: S,
}

impl StreamRequest<BodyDataStream<IncomingBody>> {
  pub fn new(
    req: svc_request_base::BaseRequest,
  ) -> Result<StreamRequest<BodyDataStream<IncomingBody>>> {
    let (parts, body) = req.into_parts();
    let query = parts.uri.query();
    let mut query_parsed =
      form_urlencoded::parse(query.map_or(b"", str::as_bytes));
    let response_stream_id = query_parsed
      .find_map(|item| {
        if item.0 == "response_stream_id" {
          Some(Vec::from_hex(&item.1))
        } else {
          None
        }
      })
      .transpose()?;
    Ok(StreamRequest {
      method: parts.method,
      response_stream_id,
      data: body.into_data_stream(),
    })
  }
}
