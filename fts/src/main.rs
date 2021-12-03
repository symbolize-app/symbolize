mod api;
mod core;
mod db;
mod search;

use std::error::Error as StdError;
use tokio::spawn;
use tokio::task::spawn_blocking;

#[tokio::main]
async fn main(
) -> Result<(), Box<dyn StdError + Send + Sync>> {
  let search_load_future = spawn_blocking(search::load);
  let db_load_future = spawn(db::load());
  let search_context = search_load_future.await??;
  let (db_context, db_handle) = db_load_future.await??;
  api::run(search_context, db_context, db_handle).await?;
  Ok(())
}
