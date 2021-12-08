mod api;
mod core;
mod db;
mod search;

use std::error::Error as StdError;
use tokio::spawn;
use tokio::task::spawn_blocking;
use tokio::try_join;

#[tokio::main]
async fn main(
) -> Result<(), Box<dyn StdError + Send + Sync>> {
  let (search_context_map, (db_context, db_handle)) = try_join!(
    spawn_blocking(search::load_map),
    spawn(db::load())
  )?;
  api::run(search_context_map, db_context, db_handle)
    .await?;
  Ok(())
}
