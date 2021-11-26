use crate::core::message;
use crate::db;
use crate::search;
use std::collections::HashMap;
use std::env;
use std::error::Error as StdError;
use std::net;
use std::sync::Arc;

pub struct Context {
  pub search: search::Context,
  pub db: db::Context,
}

pub async fn run_server(
  context: Context,
) -> Result<(), Box<dyn StdError>> {
  let host: net::Ipv4Addr =
    env::var("FTS_HOST")?.parse()?;
  let port: u16 = env::var("FTS_PORT")?.parse()?;
  let addr = net::SocketAddr::from((host, port));

  let context = Arc::new(context);
  let service = hyper::service::service_fn(move |req| {
    handle_request(context.clone(), req)
  });
  let server = hyper::Server::bind(&addr)
    .serve(tower::make::Shared::new(service));
  server.await?;
  Ok(())
}

#[allow(clippy::unused_async)]
async fn handle_request(
  context: Arc<Context>,
  req: hyper::Request<hyper::Body>,
) -> Result<
  hyper::Response<hyper::Body>,
  Box<dyn StdError + Send + Sync>,
> {
  let searcher = context.search.index_reader.searcher();
  let query_parser = tantivy::query::QueryParser::for_index(
    &context.search.index,
    vec![
      context.search.fields.title,
      context.search.fields.body,
    ],
  );
  println!("{}", message::get_message());
  let query = query_parser.parse_query(
    url::form_urlencoded::parse(
      req.uri().query().ok_or("invalid query")?.as_bytes(),
    )
    .into_owned()
    .collect::<HashMap<String, String>>()
    .get("q")
    .ok_or("missing query param")?,
  )?;
  let top_docs = searcher.search(
    &query,
    &tantivy::collector::TopDocs::with_limit(10),
  )?;
  let mut result = "".to_string();
  for (_score, doc_address) in top_docs {
    let retrieved_doc = searcher.doc(doc_address)?;
    result.push_str(
      &context.search.schema.to_json(&retrieved_doc),
    );
  }
  Ok(hyper::Response::new(result.into()))
}
