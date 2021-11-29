use crate::core::document::Language;
use crate::core::message;
use crate::db;
use crate::search;
use std::collections::HashMap;
use std::env;
use std::error::Error as StdError;
use std::net;
use std::str::FromStr as _;
use std::sync::Arc;

pub async fn run_server(
  search_context: search::Context,
  db_context: db::Context,
) -> Result<(), Box<dyn StdError>> {
  // TODO Spawn a thread and use a semaphore (max 1) to communicate reindexing needed
  let host: net::Ipv4Addr =
    env::var("FTS_HOST")?.parse()?;
  let port: u16 = env::var("FTS_PORT")?.parse()?;
  let addr = net::SocketAddr::from((host, port));

  let search_context = Arc::new(search_context);
  let db_context = Arc::new(db_context);
  let service = hyper::service::service_fn(move |req| {
    handle_request(
      search_context.clone(),
      db_context.clone(),
      req,
    )
  });
  let server = hyper::Server::bind(&addr)
    .serve(tower::make::Shared::new(service));
  server.await?;
  Ok(())
}

#[allow(clippy::unused_async)]
async fn handle_request(
  search_context: Arc<search::Context>,
  db_context: Arc<db::Context>,
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
  let search_context = search_context.as_ref();
  let _db_context = db_context.as_ref();
  let instance = search_context
    .instances
    .get(&language)
    .ok_or("Invalid language")?;
  let searcher = instance.index_reader.searcher();
  let query_parser = tantivy::query::QueryParser::for_index(
    &instance.index,
    vec![
      search_context.fields.title,
      search_context.fields.body,
    ],
  );
  println!("{}", message::get_message());
  let query = query_parser.parse_query(query)?;
  let top_docs = searcher.search(
    &query,
    &tantivy::collector::TopDocs::with_limit(10),
  )?;
  let mut result = "".to_string();
  for (_score, doc_address) in top_docs {
    let retrieved_doc = searcher.doc(doc_address)?;
    result.push_str(
      &search_context.schema.to_json(&retrieved_doc),
    );
  }
  Ok(hyper::Response::new(result.into()))
}
