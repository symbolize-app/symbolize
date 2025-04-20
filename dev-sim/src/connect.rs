use crate::command as svc_command;
use crate::context as svc_context;
use anyhow::anyhow;
use anyhow::Result;
use bytes::Bytes;
use http_body_util::Empty;
use hyper::client::conn::http2;
use hyper::client::conn::http2::SendRequest;
use hyper_util::rt::TokioExecutor;
use hyper_util::rt::TokioIo;
use rustls::pki_types::IpAddr;
use rustls::pki_types::ServerName;
use rustls::ClientConfig as TlsClientConfig;
use rustls::RootCertStore;
use rustls_pemfile::certs;
use rustyline_async::SharedWriter;
use std::env;
use std::io::Write as _;
use std::net::Ipv6Addr;
use std::net::SocketAddr;
use std::sync::Arc;
use std::vec;
use tokio;
use tokio::fs::read;
use tokio::net::TcpStream;
use tokio::sync::watch;
use tokio_rustls::TlsConnector;

struct Context {
  stdout: SharedWriter,
  id: usize,
  sender_tx: watch::Sender<Option<Result<SendRequest<Empty<Bytes>>>>>,
}

pub fn run(
  ctx: &mut svc_context::Context,
) -> Result<svc_command::CommandStatus> {
  let (sender_tx, sender_rx) = watch::channel(None);
  let id = ctx.connections.len();
  let sub_ctx = Context {
    stdout: ctx.stdout.clone(),
    id,
    sender_tx: sender_tx.clone(),
  };
  writeln!(ctx.stdout, "[c{id}] Connecting...")?;
  let sim_connection = svc_context::SimConnection {
    task: tokio::spawn(try_connect(sub_ctx)),
    sender_tx,
    sender_rx,
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
      writeln!(ctx.stdout, "[c{id}] Connect error: {err}")?;
      ctx.sender_tx.send(Some(Err(anyhow!("[c{id}] {err}"))))?;
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
  let sender: SendRequest<Empty<Bytes>> = sender;
  ctx.sender_tx.send(Some(Ok(sender)))?;
  conn.await?;
  writeln!(ctx.stdout, "[c{id}] Disconnected")?;
  ctx
    .sender_tx
    .send(Some(Err(anyhow!("[c{id}] disconnected"))))?;
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
