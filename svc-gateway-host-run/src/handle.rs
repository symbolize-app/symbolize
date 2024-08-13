use crate::db as svc_db;
use crate::db::Db as _;
use crate::random as svc_random;
use crate::random::RandomExt;
use crate::request as svc_request;
use crate::response as svc_response;
use crate::state as svc_state;
use crate::state::StateExt;
use anyhow::Error;
use anyhow::Result;
use futures::stream;
use futures::StreamExt;
use futures::TryFutureExt as _;
use futures::TryStreamExt as _;
use http::StatusCode;
use http_body_util::BodyDataStream;
use hyper::body::Incoming as IncomingBody;
use intertwine_lib_hex::FromHex as _;
use std::time::Duration;
use tokio;
use tokio::sync::mpsc;
use tokio::time::sleep;
use tokio_stream::wrappers as stream_wrappers;

const CODE_ID_PREFIX: &str = "/.code/.id/";
const CODE_PREFIX: &str = "/.code/";
const STREAM_PATH: &str = "/.stream";
const INIT_HTML_PATH: &str = "svc-gateway-guest-run/init.html";
const RESPONSE_STREAM_CHANNEL_BUFFER_SIZE: usize = 16;
const RESPONSE_STREAM_CHANNEL_SEND_TIMEOUT: Duration =
  Duration::from_millis(1_000);
const RESPONSE_STREAM_IDLE_TIMEOUT: Duration =
  Duration::from_millis(5_000);

pub async fn handle<TContext>(
  ctx: &TContext,
  req: svc_request::BaseRequest,
) -> Result<svc_response::BaseResponse>
where
  TContext: svc_db::Context,
  TContext: svc_random::Context,
  TContext: svc_state::Context,
{
  let result = if req.uri().path() == STREAM_PATH {
    handle_stream(ctx, req).await
  } else {
    handle_content(ctx, req).await
  };
  result.or_else(handle_error)
}

#[allow(clippy::print_stderr)]
fn handle_error(err: Error) -> Result<svc_response::BaseResponse> {
  let err = if err.is::<svc_response::Error>() {
    err.downcast().expect("downcast")
  } else {
    eprintln!("Internal error {err:?}");
    svc_response::Error::new(
      StatusCode::INTERNAL_SERVER_ERROR,
      "Internal error",
    )
  };
  err.into_response()
}

async fn handle_content<TContext>(
  ctx: &TContext,
  req: svc_request::BaseRequest,
) -> Result<svc_response::BaseResponse>
where
  TContext: svc_db::Context,
{
  let req = svc_request::DataRequest::new(req)?;
  let content_response =
    if let Some(path) = req.path.strip_prefix(CODE_ID_PREFIX) {
      let path = path.to_owned();
      handle_content_by_id(
        ctx,
        svc_request::ContentRequest::try_new(req, path)?,
      )
      .await?
    } else if let Some(path) = req.path.strip_prefix(CODE_PREFIX) {
      let path = path.to_owned();
      handle_content_by_path(
        ctx,
        svc_request::ContentRequest::try_new(req, path)?,
      )
      .await?
    } else if &req.path == "/" {
      handle_content_by_path(
        ctx,
        svc_request::ContentRequest::try_new_no_sandbox(
          req,
          INIT_HTML_PATH.to_owned(),
        )?,
      )
      .await?
    } else {
      None
    };
  if let Some(response) = content_response {
    response.into_response()
  } else {
    Err(
      svc_response::Error::new(StatusCode::NOT_FOUND, "Not found").into(),
    )
  }
}

#[allow(clippy::print_stderr)]
async fn handle_content_by_id<TContext>(
  ctx: &TContext,
  req: svc_request::ContentRequest,
) -> Result<Option<svc_response::ContentResponse>>
where
  TContext: svc_db::Context,
{
  let content_id = Vec::from_hex(&req.path_prefix).map_err(|_| {
    eprintln!("  invalid content ID hex");
    svc_response::Error::new(
      StatusCode::BAD_REQUEST,
      "invalid content ID hex",
    )
  })?;
  let original = ctx.db().get_content_by_id(content_id.clone()).await?;
  if let Some(original) = original {
    Ok(Some(svc_response::ContentResponse::try_new_immutable(
      req,
      svc_db::ContentRowWithId {
        id: content_id,
        original,
      },
    )?))
  } else {
    Ok(None)
  }
}

async fn handle_content_by_path<TContext>(
  ctx: &TContext,
  req: svc_request::ContentRequest,
) -> Result<Option<svc_response::ContentResponse>>
where
  TContext: svc_db::Context,
{
  let path_id = req.full_path.clone();
  let content_row = ctx.db().get_content_by_path(path_id).await?;
  if let Some(content_row) = content_row {
    Ok(Some(svc_response::ContentResponse::try_new(
      req,
      content_row,
    )?))
  } else {
    Ok(None)
  }
}

#[allow(clippy::print_stdout)]
async fn handle_stream<TContext>(
  ctx: &TContext,
  req: svc_request::BaseRequest,
) -> Result<svc_response::BaseResponse>
where
  TContext: svc_db::Context,
  TContext: svc_random::Context,
  TContext: svc_state::Context,
{
  let req = svc_request::StreamRequest::new(req)?;
  if let Some(response_stream_id) = &req.response_stream_id {
    println!("Request with response stream ID {response_stream_id:?}");
    let response_stream_state = ctx
      .state()
      .find_response_stream(response_stream_id)
      .ok_or_else(|| {
        svc_response::Error::new(
          StatusCode::NOT_FOUND,
          "response stream not found",
        )
      })?;
    handle_stream_request(req, response_stream_state).await?;
    svc_response::StreamResponse {
      response_stream_id: None,
      body: stream::empty::<Result<&[u8]>>(),
    }
    .into_response()
  } else {
    let response_stream_id = ctx.random().response_stream_id()?.expose();
    println!("New response stream ID {response_stream_id:?}");
    let (response_stream_sender, response_stream_receiver) =
      mpsc::channel(RESPONSE_STREAM_CHANNEL_BUFFER_SIZE);
    let (response_stream_close_sender, mut response_stream_close_receiver) =
      mpsc::channel(1);
    let response_stream_state = svc_state::ResponseStreamState {
      sender: response_stream_sender,
      close: response_stream_close_sender,
    };
    ctx.state().register_response_stream(
      &response_stream_id,
      &response_stream_state,
    )?;
    let request_stream_handle = tokio::spawn(async move {
      handle_stream_request(req, response_stream_state).await
    });
    let response_stream_future =
      request_stream_handle.map_ok(|_| vec![]).map_err(Error::new);
    svc_response::StreamResponse {
      response_stream_id: Some(response_stream_id),
      body: stream_wrappers::ReceiverStream::new(response_stream_receiver)
        .take_until(async move {
          response_stream_close_receiver.recv().await;
        })
        .map(Ok)
        .chain(stream::once(response_stream_future)),
    }
    .into_response()
  }
}

#[allow(clippy::print_stdout)]
async fn handle_stream_request(
  req: svc_request::StreamRequest<BodyDataStream<IncomingBody>>,
  response_stream_state: svc_state::ResponseStreamState,
) -> Result<()> {
  let result = req
    .data
    .map_err(Error::new)
    .try_for_each(|data| {
      response_stream_state
        .sender
        .send_timeout(
          data.to_ascii_uppercase(),
          RESPONSE_STREAM_CHANNEL_SEND_TIMEOUT,
        )
        .map_err(Error::new)
    })
    .await;
  tokio::spawn(async move {
    sleep(RESPONSE_STREAM_IDLE_TIMEOUT).await;
    if response_stream_state.sender.strong_count() == 2 {
      let _ = response_stream_state.close.send(()).await;
    }
  });
  result
}

#[cfg(test)]
#[path = "./handle_test.rs"]
mod handle_test;
