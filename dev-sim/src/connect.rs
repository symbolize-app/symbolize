use crate::cancel_receiver_stream as svc_cancel_receiver_stream;
use crate::command as svc_command;
use crate::context as svc_context;
use anyhow::Result;
use anyhow::anyhow;
use futures::StreamExt;
use hyper::client::conn::http2;
use hyper::client::conn::http2::SendRequest;
use hyper_util::rt::TokioExecutor;
use hyper_util::rt::TokioIo;
use rustls::ClientConfig as TlsClientConfig;
use rustls::RootCertStore;
use rustls::pki_types::IpAddr;
use rustls::pki_types::ServerName;
use rustls_pemfile::certs;
use rustyline_async::SharedWriter;
use std::env;
use std::io::Write as _;
use std::net::Ipv6Addr;
use std::net::SocketAddr;
use std::pin::pin;
use std::sync::Arc;
use std::vec;
use tokio;
use tokio::fs::read;
use tokio::net::TcpStream;
use tokio::sync::mpsc;
use tokio_rustls::TlsConnector;
use tokio_util::sync::CancellationToken;

struct Context {
  stdout: SharedWriter,
  id: usize,
  cancellation_token: CancellationToken,
  send_request_rx: Option<mpsc::Receiver<svc_context::SimRequest>>,
}

pub fn run(
  ctx: &mut svc_context::Context,
) -> Result<svc_command::CommandStatus> {
  let id = ctx.connections.len();
  let cancellation_token = CancellationToken::new();
  let (send_request_tx, send_request_rx) = mpsc::channel(128);
  let sub_ctx = Context {
    stdout: ctx.stdout.clone(),
    id,
    cancellation_token: cancellation_token.clone(),
    send_request_rx: Some(send_request_rx),
  };
  writeln!(ctx.stdout, "[c{id}] Connecting...")?;
  let sim_connection = svc_context::SimConnection {
    task: tokio::spawn(try_connect(sub_ctx)),
    cancellation_token,
    send_request_tx,
    selected_stream_id: 0,
    streams: vec![],
  };
  ctx.selected_connection_id = id;
  ctx.connections.push(sim_connection);
  Ok(svc_command::CommandStatus::Continue)
}

async fn try_connect(mut ctx: Context) -> Result<()> {
  let id = ctx.id;
  match connect(&mut ctx).await {
    Ok(()) => {}
    Err(err) => {
      ctx.cancellation_token.cancel();
      writeln!(ctx.stdout, "[c{id}] Connect error: {err}")?;
    }
  }
  Ok(())
}

async fn connect(ctx: &mut Context) -> Result<()> {
  let id = ctx.id;

  let loopback = Ipv6Addr::new(0, 0, 0, 0, 0, 0, 0, 1);
  let port: u16 = env::var("APP_DEV_PORT")?.parse()?;
  let addr = SocketAddr::new(loopback.into(), port);
  let stream = TcpStream::connect(addr).await?;

  let tls_client_config = build_tls_client_config().await?;
  let connector = TlsConnector::from(Arc::new(tls_client_config));
  let name = ServerName::IpAddress(IpAddr::V6(loopback.into()));
  let stream = connector.connect(name, stream).await?;

  let executor = TokioExecutor::new();
  let builder = http2::Builder::new(executor);
  let io = TokioIo::new(stream);
  let (sender, conn) = builder.handshake(io).await?;
  writeln!(ctx.stdout, "[c{id}] Connected")?;

  let conn_task = tokio::spawn(conn);
  handle_requests(ctx, sender).await?;

  conn_task.await??;
  writeln!(ctx.stdout, "[c{id}] Disconnected")?;

  Ok(())
}

async fn build_tls_client_config() -> Result<TlsClientConfig> {
  let mut root_cert_store = RootCertStore::empty();
  let mut local_certs_slice = &read(".pki/ca.crt").await?[..];
  let local_certs = certs(&mut local_certs_slice)
    .map(|item| item.map_err(Into::into))
    .collect::<Result<Vec<_>>>()?;
  for cert in local_certs {
    root_cert_store.add(cert)?;
  }
  Ok(
    TlsClientConfig::builder()
      .with_root_certificates(root_cert_store)
      .with_no_client_auth(),
  )
}

async fn handle_requests(
  ctx: &mut Context,
  mut sender: SendRequest<svc_context::SimBody>,
) -> Result<()> {
  let send_request_rx = ctx
    .send_request_rx
    .take()
    .ok_or_else(|| anyhow!("missing send request RX"))?;
  let mut send_request_stream =
    pin!(svc_cancel_receiver_stream::CancelReceiverStream::new(
      ctx.cancellation_token.clone(),
      send_request_rx
    ));
  while let Some((req, res_tx)) = send_request_stream.next().await {
    let res = sender.send_request(req).await?;
    res_tx
      .send(res)
      .map_err(|_| anyhow!("error sending response"))?;
  }
  Ok(())
}
