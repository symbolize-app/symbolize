use crate::context as svc_context;
use crate::executor as svc_executor;
use crate::handle as svc_handle;
use crate::state::Context;
use crate::state::State;
use anyhow::Result;
use anyhow::anyhow;
use hyper::server::conn::http2;
use hyper::service::service_fn;
use hyper_util::rt::TokioIo;
use rustls::ServerConfig as TlsServerConfig;
use rustls_pemfile::certs;
use rustls_pemfile::pkcs8_private_keys;
use std::env;
use std::net::Ipv6Addr;
use std::net::SocketAddr;
use std::sync::Arc;
use tokio::fs::read;
use tokio::net::TcpListener;
use tokio::net::TcpStream;
use tokio::signal::unix::SignalKind;
use tokio::signal::unix::signal;
use tokio_rustls::TlsAcceptor;
use tokio_rustls::server::TlsStream;
use tokio_stream::StreamExt as _;
use tokio_stream::wrappers::TcpListenerStream;
use tokio_util::task::TaskTracker;

pub async fn serve(ctx: Arc<svc_context::ContextImpl>) -> Result<()> {
  let tls_server_config = Arc::new(build_tls_server_config().await?);

  tokio::spawn(serve_tcp_accept(ctx.clone(), tls_server_config));

  receive_termination_signal(ctx).await?;

  Ok(())
}

#[allow(clippy::print_stdout)]
async fn receive_termination_signal(
  ctx: Arc<svc_context::ContextImpl>,
) -> Result<()> {
  let mut sigint = signal(SignalKind::interrupt())?;
  if sigint.recv().await.is_some() {
    println!("Received interrupt signal, shutting down...");
    ctx.state().cancellation_token().cancel();
  }
  Ok(())
}

async fn build_tls_server_config() -> Result<TlsServerConfig> {
  let mut local_certs_slice =
    &read(".pki/issued/symbolize-gateway.crt").await?[..];
  let local_certs = certs(&mut local_certs_slice)
    .map(|item| item.map_err(Into::into))
    .collect::<Result<Vec<_>>>()?;

  let mut local_keys_slice =
    &read(".pki/private/symbolize-gateway.key").await?[..];
  let local_key = pkcs8_private_keys(&mut local_keys_slice)
    .next()
    .ok_or(anyhow!("missing key"))
    .and_then(|result| result.map_err(Into::into))?;

  let mut config = TlsServerConfig::builder()
    .with_no_client_auth()
    .with_single_cert(local_certs, local_key.into())?;
  config.alpn_protocols = vec![b"h2".to_vec()];

  Ok(config)
}

#[allow(clippy::print_stdout)]
#[allow(clippy::print_stderr)]
async fn serve_tcp_accept(
  ctx: Arc<svc_context::ContextImpl>,
  tls_server_config: Arc<TlsServerConfig>,
) -> Result<()> {
  let loopback = Ipv6Addr::new(0, 0, 0, 0, 0, 0, 0, 1);
  let port: u16 = env::var("APP_DEV_PORT")?.parse()?;
  let addr = SocketAddr::new(loopback.into(), port);
  let mut tcp_listener_stream =
    TcpListenerStream::new(TcpListener::bind(addr).await?);
  println!("Serving at https://{addr:}");

  let connection_task_tracker = TaskTracker::new();

  while let Some(tcp_stream_result) = tcp_listener_stream.next().await {
    match tcp_stream_result {
      Ok(tcp_stream) => {
        let ctx = ctx.clone();
        let tls_server_config = tls_server_config.clone();
        connection_task_tracker.spawn(serve_tls_accept(
          ctx,
          tls_server_config,
          tcp_stream,
        ));
      }
      Err(err) => {
        eprintln!("Error accepting TCP connection: {err:?}");
      }
    }
  }

  connection_task_tracker.wait().await;

  Ok(())
}

#[allow(clippy::print_stderr)]
async fn serve_tls_accept(
  ctx: Arc<svc_context::ContextImpl>,
  tls_server_config: Arc<TlsServerConfig>,
  tcp_stream: TcpStream,
) {
  let tls_stream_result = TlsAcceptor::from(tls_server_config)
    .accept(tcp_stream)
    .await;
  match tls_stream_result {
    Ok(tls_stream) => {
      serve_http2(ctx, tls_stream).await;
    }
    Err(err) => {
      eprintln!("Error accepting TLS stream: {err:?}");
    }
  }
}

#[allow(clippy::print_stderr)]
async fn serve_http2(
  ctx: Arc<svc_context::ContextImpl>,
  tls_stream: TlsStream<TcpStream>,
) {
  let tcp_io = TokioIo::new(tls_stream);
  let request_task_tracker = TaskTracker::new();
  let service = service_fn(|req| {
    let ctx = ctx.clone();
    async move { svc_handle::handle(ctx.as_ref(), req).await }
  });
  let executor =
    svc_executor::Executor::new(ctx.clone(), &request_task_tracker);
  let connection_result = http2::Builder::new(executor)
    .serve_connection(tcp_io, service)
    .await;
  if let Err(err) = connection_result {
    eprintln!("Error serving connection: {err:?}");
  }
  request_task_tracker.wait().await;
}
