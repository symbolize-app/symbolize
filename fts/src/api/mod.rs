use crate::core::document::Language;
use crate::db;
use crate::search;
use rand::rngs::StdRng;
use rand::Rng as _;
use rand::SeedableRng;
use std::collections::HashMap;
use std::env;
use std::error::Error as StdError;
use std::net;
use std::str::FromStr as _;
use std::sync::atomic::AtomicU64;
use std::sync::Arc;
use std::time::Duration;
use time::format_description::well_known::Rfc3339;
use time::OffsetDateTime;
use tokio::spawn;
use tokio::sync::Notify;
use tokio::sync::Semaphore;
use tokio::task::spawn_blocking;
use tokio::time::sleep;
use tokio::try_join;

pub struct Context {
  pub cpu_available: Semaphore,
  pub search_update_requested: Notify,
  pub search_updated_unix_timestamp: AtomicU64,
}

pub async fn run(
  search_context_map: Arc<
    HashMap<Language, Arc<search::Context>>,
  >,
  db_context: Arc<db::Context>,
  db_handle: db::Handle,
) -> Result<(), Box<dyn StdError + Send + Sync>> {
  let api_context = Arc::new(Context {
    cpu_available: Semaphore::new(num_cpus::get()),
    search_update_requested: Notify::new(),
    search_updated_unix_timestamp: AtomicU64::from(0),
  });
  let periodic_search_update_handle =
    spawn(periodic_search_update(api_context.clone()));
  let run_search_update_handle = spawn(run_search_update(
    api_context.clone(),
    search_context_map.clone(),
    db_context.clone(),
  ));
  let run_server_handle = spawn(run_server(
    api_context.clone(),
    search_context_map.clone(),
  ));
  try_join!(
    periodic_search_update_handle,
    run_search_update_handle,
    run_server_handle,
    db_handle.connection_handle
  )?;
  Ok(())
}

const SEARCH_UPDATE_BUFFER_SECONDS: i64 = 5;

async fn periodic_search_update(api_context: Arc<Context>) {
  let mut rng = StdRng::from_entropy();
  loop {
    api_context.search_update_requested.notify_one();
    sleep(Duration::from_secs_f64(
      rng.gen_range(60.0..120.0),
    ))
    .await;
  }
}

async fn run_search_update(
  api_context: Arc<Context>,
  search_context_map: Arc<
    HashMap<Language, Arc<search::Context>>,
  >,
  db_context: Arc<db::Context>,
) {
  let result: Result<(), Box<dyn StdError + Send + Sync>> =
    (|| async {
      loop {
        api_context
          .search_update_requested
          .notified()
          .await;
        for (language, search_context) in
          search_context_map.iter()
        {
          run_search_update_for_language(
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

async fn run_search_update_for_language(
  api_context: &Arc<Context>,
  language: Language,
  search_context: &Arc<search::Context>,
  db_context: &Arc<db::Context>,
) -> Result<(), Box<dyn StdError + Send + Sync>> {
  let updated_at =
    *search_context.updated_at.read().await;
  let new_current_at = OffsetDateTime::now_utc();
  let documents = db::find_recent_updates(
    db_context,
    SEARCH_UPDATE_BUFFER_SECONDS,
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
      spawn_blocking(move ||
        search::update(
          search_context,
          new_updated_at,
          new_current_at,
          documents,
        )
      )
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

async fn run_server(
  _api_context: Arc<Context>,
  search_context_map: Arc<
    HashMap<Language, Arc<search::Context>>,
  >,
) {
  let result: Result<(), Box<dyn StdError + Send + Sync>> =
    (|| async {
      let host: net::Ipv4Addr =
        env::var("FTS_HOST")?.parse()?;
      let port: u16 = env::var("FTS_PORT")?.parse()?;
      let addr = net::SocketAddr::from((host, port));
      let service =
        hyper::service::service_fn(move |req| {
          handle_request(search_context_map.clone(), req)
        });
      let server = hyper::Server::bind(&addr)
        .serve(tower::make::Shared::new(service));
      server.await?;
      Ok(())
    })()
    .await;
  result.expect("server error");
}

#[allow(clippy::unused_async)]
async fn handle_request(
  search_context_map: Arc<
    HashMap<Language, Arc<search::Context>>,
  >,
  req: hyper::Request<hyper::Body>,
) -> Result<
  hyper::Response<hyper::Body>,
  Box<dyn StdError + Send + Sync>,
> {
  let params = url::form_urlencoded::parse(
    req.uri().query().ok_or("invalid query")?.as_bytes(),
  )
  .into_owned()
  .collect::<HashMap<String, String>>();
  let language =
    params.get("l").ok_or("missing language param")?;
  let language: Language = Language::from_str(language)?;
  let query =
    params.get("q").ok_or("missing query param")?;
  let search_context_map = search_context_map.as_ref();
  let search_context = search_context_map
    .get(&language)
    .ok_or("invalid language")?;
  let searcher = search_context.index_reader.searcher();
  let query_parser = tantivy::query::QueryParser::for_index(
    &search_context.index,
    vec![
      search_context.fields.title,
      search_context.fields.names,
      search_context.fields.content,
    ],
  );
  let query = query_parser.parse_query(query)?;
  let top_docs = searcher.search(
    &query,
    &tantivy::collector::TopDocs::with_limit(10),
  )?;
  let mut result = String::new();
  for (_score, doc_address) in top_docs {
    let retrieved_doc = searcher.doc(doc_address)?;
    result.push_str(
      &search_context.schema.to_json(&retrieved_doc),
    );
  }
  Ok(hyper::Response::new(result.into()))
}
