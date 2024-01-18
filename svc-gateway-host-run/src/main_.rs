use crate::context as svc_context;
use crate::db as svc_db;
use crate::handle as svc_handle;
use anyhow::Result;
use hyper::server::conn::http1;
use hyper::service::service_fn;
use hyper_util::rt::TokioIo;
use std::env;
use std::net::Ipv6Addr;
use std::net::SocketAddr;
use std::sync::Arc;
use tokio::net::TcpListener;
use tokio::task::spawn;
use tokio_stream::wrappers::TcpListenerStream;
use tokio_stream::StreamExt as _;

#[allow(clippy::print_stdout)]
#[allow(clippy::print_stderr)]
#[tokio::main]
pub async fn main() -> Result<()> {
  let ctx = Arc::new(svc_context::MainContext {
    db: svc_db::MainContext::init().await?,
  });
  println!("Opened");

  let port: u16 = env::var("APP_DEV_PORT")?.parse()?;
  let loopback = Ipv6Addr::new(0, 0, 0, 0, 0, 0, 0, 1);
  let addr = SocketAddr::new(loopback.into(), port);
  let mut tcp_listener_stream =
    TcpListenerStream::new(TcpListener::bind(addr).await?);
  println!("Serving at http://{addr:}");

  while let Some(tcp_stream_result) = tcp_listener_stream.next().await {
    match tcp_stream_result {
      Ok(tcp_stream) => {
        let tcp_io = TokioIo::new(tcp_stream);
        let ctx = ctx.clone();

        spawn(async move {
          let service =
            service_fn(|req| svc_handle::handle(ctx.as_ref(), req));
          if let Err(err) = http1::Builder::new()
            .serve_connection(tcp_io, service)
            .await
          {
            eprintln!("Error serving connection: {err:?}");
          }
        });
      }
      Err(err) => {
        eprintln!("Error accepting connection: {err:?}");
      }
    }
  }

  Ok(())
}
