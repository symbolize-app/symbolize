use hyper::service::make_service_fn;
use hyper::service::service_fn;
use hyper::Body;
use hyper::Request;
use hyper::Response;
use hyper::Server;
use std::convert::Infallible;
use std::error::Error;
use std::net::SocketAddr;

#[tokio::main]
async fn main() -> Result<(), Box<dyn Error>> {
  let addr = SocketAddr::from(([127, 0, 0, 1], 3000));

  let make_service = make_service_fn(move |_| async {
    // https://github.com/hyperium/hyper/blob/ed2fdb7b6a2963cea7577df05ddc41c56fee7246/examples/state.rs
    Ok::<_, Infallible>(service_fn(hello_world))
  });

  let server = Server::bind(&addr).serve(make_service);

  server.await?;

  Ok(())
}

async fn hello_world(
  _req: Request<Body>,
) -> Result<Response<Body>, Infallible> {
  Ok(Response::new("Hello, World".into()))
}
