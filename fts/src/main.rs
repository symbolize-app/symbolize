mod api;
mod core;
mod db;
mod search;

use std::error::Error as StdError;

#[tokio::main]
async fn main() -> Result<(), Box<dyn StdError>> {
  let search_context = search::load()?;
  let db_context = db::load().await?;
  search::update(&search_context)?;
  api::run_server(search_context, db_context).await?;
  Ok(())
}
