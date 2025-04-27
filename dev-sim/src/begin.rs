use crate::command as svc_command;
use crate::context as svc_context;
use anyhow::anyhow;
use anyhow::Result;
use bytes::Bytes;
use http_body_util::Empty;
use hyper::client::conn::http2::SendRequest;
use hyper::Request;
use rustyline_async::SharedWriter;
use std::io::Write as _;
use tokio;
use tokio::sync::watch;

struct Context {
  stdout: SharedWriter,
  connection_id: usize,
  id: usize,
  sender_rx: watch::Receiver<Option<Result<SendRequest<Empty<Bytes>>>>>,
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
  let sub_ctx = Context {
    stdout: ctx.stdout.clone(),
    connection_id: cid,
    id,
    sender_rx: sim_connection.sender_rx.clone(),
  };
  writeln!(ctx.stdout, "[c{cid}_s{id}] Beginning...")?;
  let sim_stream = svc_context::SimStream {
    task: tokio::spawn(try_connect(sub_ctx)),
  };
  sim_connection.selected_stream_id = id;
  sim_connection.streams.push(sim_stream);
  Ok(svc_command::CommandStatus::Continue)
}

async fn try_connect(mut ctx: Context) -> Result<()> {
  let cid = ctx.connection_id;
  let id = ctx.id;
  match connect(&mut ctx).await {
    Ok(()) => {}
    Err(err) => {
      writeln!(ctx.stdout, "[c{cid}_s{id}] Begin error: {err}")?;
    }
  }
  Ok(())
}

async fn connect(ctx: &mut Context) -> Result<()> {
  let cid = ctx.connection_id;
  let id = ctx.id;
  let mut sender = borrow_sender(ctx).await?;
  let request = Request::builder()
    .method("GET")
    .uri("/")
    .body(Empty::<Bytes>::new())?;
  let res = sender.send_request(request).await?;
  let status = res.status();
  let headers = res.headers();
  writeln!(ctx.stdout, "[c{cid}_s{id}] Begin request status: {status}")?;
  writeln!(
    ctx.stdout,
    "[c{cid}_s{id}] Begin request headers: {headers:#?}"
  )?;
  Ok(())
}

async fn borrow_sender(
  ctx: &mut Context,
) -> Result<SendRequest<Empty<Bytes>>> {
  let mut sender_rx = ctx.sender_rx.clone();
  loop {
    if let Some(result) = sender_rx.borrow_and_update().as_ref() {
      return Ok(result.as_ref().map_err(|err| anyhow!("{err}"))?.clone());
    }
    sender_rx.changed().await?;
  }
}
