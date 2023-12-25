use crate::context as svc_context;
use crate::db as svc_db;
use crate::handle as svc_handle;
use anyhow::Result;
use hyper::server::conn::http1;
use hyper::service::service_fn;
use hyper_util::rt::TokioIo;
use std::env;
use std::net::SocketAddr;
use std::sync::Arc;
use std::sync::Mutex;
use tokio::net::TcpListener;
use tokio::task::spawn;

#[allow(clippy::print_stdout)]
#[tokio::main]
pub async fn main() -> Result<()> {
  let db = Arc::new(Mutex::new(svc_db::init_db_context().await?));
  let ctx = svc_context::Context { db };
  let service =
    service_fn(move |req| svc_handle::handle(ctx.clone(), req));
  println!("Opened");

  let port: u16 = env::var("APP_DEV_PORT")?.parse()?;
  let addr = SocketAddr::from(([127, 0, 0, 1], port));
  let listener = TcpListener::bind(addr).await?;
  println!("Serving at http://{addr:}");

  loop {
    let (stream, _) = listener.accept().await?;
    let io = TokioIo::new(stream);
    let service = service.clone();

    spawn(async move {
      if let Err(err) =
        http1::Builder::new().serve_connection(io, service).await
      {
        println!("Error serving connection: {err:?}");
      }
    });
  }
}
