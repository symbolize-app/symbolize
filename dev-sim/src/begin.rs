use crate::cancel_receiver_stream as svc_cancel_receiver_stream;
use crate::command as svc_command;
use crate::context as svc_context;
use anyhow::anyhow;
use anyhow::Error;
use anyhow::Result;
use bytes::Bytes;
use futures::StreamExt as _;
use futures::TryStreamExt as _;
use http_body::Frame;
use http_body_util::BodyExt as _;
use http_body_util::Empty;
use http_body_util::StreamBody;
use hyper::body::Incoming as IncomingBody;
use hyper::Request;
use hyper::StatusCode;
use rustyline_async::SharedWriter;
use std::io::Write as _;
use symbolize_lib_hex::FromHex as _;
use symbolize_lib_hex::ToHex;
use tokio;
use tokio::sync::mpsc;
use tokio::sync::oneshot;
use tokio_util::sync::CancellationToken;

struct Context {
  stdout: SharedWriter,
  connection_id: usize,
  id: usize,
  cancellation_token: CancellationToken,
  send_request_tx: mpsc::Sender<svc_context::SimRequest>,
  send_message_rx: Option<mpsc::Receiver<String>>,
}

pub fn run(
  ctx: &mut svc_context::Context,
) -> Result<svc_command::CommandStatus> {
  let cid = ctx.selected_connection_id;
  let sim_connection = ctx
    .connections
    .get_mut(cid)
    .ok_or_else(|| anyhow!("no connections"))?;
  let id = sim_connection.streams.len();
  let cancellation_token = sim_connection.cancellation_token.child_token();
  let (send_message_tx, send_message_rx) = mpsc::channel(128);
  let sub_ctx = Context {
    stdout: ctx.stdout.clone(),
    connection_id: cid,
    id,
    cancellation_token: cancellation_token.clone(),
    send_request_tx: sim_connection.send_request_tx.clone(),
    send_message_rx: Some(send_message_rx),
  };
  writeln!(ctx.stdout, "[c{cid}_s{id}] Beginning...")?;
  let sim_stream = svc_context::SimStream {
    task: tokio::spawn(try_begin(sub_ctx)),
    cancellation_token,
    send_message_tx,
  };
  sim_connection.selected_stream_id = id;
  sim_connection.streams.push(sim_stream);
  Ok(svc_command::CommandStatus::Continue)
}

async fn try_begin(mut ctx: Context) -> Result<()> {
  let cid = ctx.connection_id;
  let id = ctx.id;
  match connect_response(&mut ctx).await {
    Ok(()) => {}
    Err(err) => {
      ctx.cancellation_token.cancel();
      writeln!(ctx.stdout, "[c{cid}_s{id}] Begin error: {err}")?;
    }
  }
  Ok(())
}

async fn connect_response(ctx: &mut Context) -> Result<()> {
  let cid = ctx.connection_id;
  let id = ctx.id;

  let (res_tx, res_rx) = oneshot::channel();
  let body: svc_context::SimBody =
    Box::pin(Empty::<Bytes>::new().map_err(Error::new));
  let request = Request::builder()
    .method("POST")
    .uri("/.stream")
    .body(body)?;
  ctx.send_request_tx.send((request, res_tx)).await?;
  let res = res_rx.await?;

  match res.status() {
    StatusCode::OK => Ok(()),
    other => Err(anyhow!("bad response {other}")),
  }?;

  let headers = res.headers();
  let response_stream_id_hex = headers
    .get("response-stream-id")
    .ok_or_else(|| anyhow!("missing response stream ID"))?
    .to_str()?;
  let response_stream_id = Vec::from_hex(response_stream_id_hex)?;
  writeln!(
    ctx.stdout,
    "[c{cid}_s{id}] Response stream begun ({response_stream_id_hex})"
  )?;

  let response_task = tokio::spawn(handle_response_messages(
    ctx.stdout.clone(),
    ctx.connection_id,
    ctx.id,
    res.into_body(),
  ));

  connect_request(ctx, response_stream_id).await?;

  response_task.await??;

  writeln!(ctx.stdout, "[c{cid}_s{id}] Response stream ended")?;

  Ok(())
}

async fn handle_response_messages(
  mut stdout: SharedWriter,
  connection_id: usize,
  id: usize,
  body: IncomingBody,
) -> Result<()> {
  let cid = connection_id;

  let mut stream = body.into_data_stream();

  while let Some(data) = stream.try_next().await? {
    let message = String::from_utf8(data.to_vec())?;
    if !message.is_empty() {
      writeln!(stdout, "[c{cid}_s{id}] RX '{message}'")?;
    }
  }

  Ok(())
}

async fn connect_request(
  ctx: &mut Context,
  response_stream_id: Vec<u8>,
) -> Result<()> {
  let cid = ctx.connection_id;
  let id = ctx.id;

  let send_message_rx = ctx
    .send_message_rx
    .take()
    .ok_or_else(|| anyhow!("missing send message RX"))?;
  let (res_tx, res_rx) = oneshot::channel();
  let mut send_message_stdout = ctx.stdout.clone();
  let send_message_stream =
    svc_cancel_receiver_stream::CancelReceiverStream::new(
      ctx.cancellation_token.clone(),
      send_message_rx,
    );
  let body: svc_context::SimBody = Box::pin(StreamBody::new(
    send_message_stream.map(move |message: String| {
      writeln!(send_message_stdout, "[c{cid}_s{id}] TX '{message}'")?;
      Ok(Frame::data(Bytes::from(message.into_bytes())))
    }),
  ));
  let response_stream_id_hex = response_stream_id.to_hex();
  let request = Request::builder()
    .method("POST")
    .uri(format!(
      "/.stream?response_stream_id={response_stream_id_hex}"
    ))
    .body(body)?;

  writeln!(ctx.stdout, "[c{cid}_s{id}] Request stream beginning...")?;
  ctx.send_request_tx.send((request, res_tx)).await?;
  let res = res_rx.await?;

  match res.status() {
    StatusCode::OK => Ok(()),
    other => Err(anyhow!("bad response {other}")),
  }?;

  res
    .into_data_stream()
    .map_ok(|_| ())
    .try_collect::<()>()
    .await?;

  writeln!(ctx.stdout, "[c{cid}_s{id}] Request stream ended")?;
  Ok(())
}
