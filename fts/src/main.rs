mod api;
mod core;
mod db;
mod search;

use std::error::Error as StdError;

#[tokio::main]
async fn main() -> Result<(), Box<dyn StdError>> {
  let context = load_context().await?;
  api::run_server(context).await?;
  Ok(())
}

async fn load_context(
) -> Result<api::Context, Box<dyn StdError>> {
  let search_context = search::load()?;
  let db_context = db::load().await?;
  Ok(api::Context {
    search: search_context,
    db: db_context,
  })
}
