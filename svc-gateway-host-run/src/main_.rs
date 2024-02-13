use crate::context as svc_context;
use crate::db as svc_db;
use crate::serve as svc_serve;
use anyhow::Result;
use std::sync::Arc;

#[allow(clippy::print_stdout)]
#[allow(clippy::print_stderr)]
#[tokio::main]
pub async fn main() -> Result<()> {
  let ctx = Arc::new(svc_context::ContextImpl {
    db: svc_db::DbImpl::init().await?,
  });
  println!("Context intialized");

  svc_serve::serve(ctx.clone()).await?;

  ctx.wait().await;

  Ok(())
}
