use crate::api;
use crate::core::document::Language;
use crate::core::result::DynResult;
use crate::db;
use crate::search;
use rand::rngs::StdRng;
use rand::Rng as _;
use rand::SeedableRng;
use std::time::Duration;
use time::format_description::well_known::Rfc3339;
use time::OffsetDateTime;
use tokio::spawn;
use tokio::task::spawn_blocking;
use tokio::time::sleep;

const BUFFER_SECONDS: f64 = 5.0;
const PERIOD_SECONDS: f64 = 120.0;

pub async fn run(
  api_context: api::Context,
  search_context_map: search::ContextMap,
  db_context: db::Context,
) {
  let result: DynResult<()> = (|| async {
    loop {
      api_context.search_update_requested.notified().await;
      for (language, search_context) in
        search_context_map.iter()
      {
        run_for_language(
          &api_context,
          *language,
          search_context,
          &db_context,
        )
        .await?;
      }
    }
  })()
  .await;
  result.expect("search update error");
}

async fn run_for_language(
  api_context: &api::Context,
  language: Language,
  search_context: &search::Context,
  db_context: &db::Context,
) -> DynResult<()> {
  let updated_at = *search_context.updated_at.read().await;
  let new_current_at = OffsetDateTime::now_utc();
  let documents = db::find_recent_updates(
    db_context,
    BUFFER_SECONDS,
    language,
    updated_at,
  )
  .await?;
  if let Some(new_updated_at) = documents
    .iter()
    .map(|document| document.updated_at)
    .max()
  {
    println!(
      "Found {:?} {} document updates ({})",
      language,
      documents.len(),
      new_updated_at.format(&Rfc3339)?
    );
    {
      let permit =
        api_context.cpu_available.acquire().await?;
      let search_context = search_context.clone();
      let new_updated_at = new_updated_at;
      let new_current_at = new_current_at;
      spawn_blocking(move || {
        search::update(
          search_context,
          new_updated_at,
          new_current_at,
          documents,
        );
      })
      .await?;
      drop(permit);
    }
    *search_context.updated_at.write().await =
      Some(new_updated_at);
    *search_context.current_at.write().await =
      Some(new_current_at);
  } else {
    println!("No {:?} document updates", language);
  }
  Ok(())
}

pub async fn periodic(api_context: api::Context) {
  let mut rng = StdRng::from_entropy();
  loop {
    api_context.search_update_requested.notify_one();
    sleep(Duration::from_secs_f64(
      rng.gen_range(PERIOD_SECONDS / 2.0..PERIOD_SECONDS),
    ))
    .await;
  }
}

pub fn handle(
  api_context: api::Context,
  req: hyper::Request<hyper::Body>,
) -> DynResult<hyper::Response<hyper::Body>> {
  if let Err(res) = api_context.auth.check_request(&req) {
    return Ok(res);
  }
  spawn(handle_queue(api_context));
  Ok(
    hyper::Response::builder()
      .status(hyper::StatusCode::ACCEPTED)
      .body("queued".into())?,
  )
}

async fn handle_queue(api_context: api::Context) {
  sleep(Duration::from_secs_f64(BUFFER_SECONDS)).await;
  api_context.search_update_requested.notify_one();
}
