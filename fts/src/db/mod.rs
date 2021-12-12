use crate::core::document::Document;
use crate::core::hex::FromHex as _;
use std::collections::HashMap;
use std::env;
use time::OffsetDateTime;
use tokio::spawn;

use crate::core::document::Language;
use crate::core::result::DynResult;
use std::sync::Arc;

#[derive(Clone)]
pub struct Context(Arc<InnerContext>);

impl std::ops::Deref for Context {
  type Target = Arc<InnerContext>;

  fn deref(&self) -> &Self::Target {
    &self.0
  }
}

pub struct InnerContext {
  pub client: tokio_postgres::Client,
}

pub struct Handle {
  pub connection_handle: tokio::task::JoinHandle<()>,
}

pub async fn load() -> (Context, Handle) {
  let result: DynResult<(Context, Handle)> = (|| async {
    let (url, params) = get_url()?;
    let tls = get_tls(&params)?;
    let config = get_config(&url, &params)?;
    let (client, connection) = config.connect(tls).await?;
    let connection_handle =
      spawn(run_connection(connection));
    Ok((
      Context(Arc::new(InnerContext { client })),
      Handle { connection_handle },
    ))
  })()
  .await;
  result.expect("DB load error")
}

async fn run_connection(
  connection: tokio_postgres::Connection<
    tokio_postgres::Socket,
    postgres_openssl::TlsStream<tokio_postgres::Socket>,
  >,
) {
  connection.await.expect("DB connection error");
}

fn get_url(
) -> DynResult<(url::Url, HashMap<String, String>)> {
  let url: url::Url =
    env::var("DATABASE_URL_API_READ")?.parse()?;
  let params = url
    .query_pairs()
    .into_owned()
    .collect::<HashMap<String, String>>();
  Ok((url, params))
}

fn get_tls(
  params: &HashMap<String, String>,
) -> DynResult<postgres_openssl::MakeTlsConnector> {
  let mut tls_builder =
    openssl::ssl::SslConnector::builder(
      openssl::ssl::SslMethod::tls(),
    )?;
  if let Some(cert) = params.get("sslcert") {
    tls_builder.set_certificate_chain_file(cert)?;
  }
  if let Some(key) = params.get("sslkey") {
    tls_builder.set_private_key_file(
      key,
      openssl::ssl::SslFiletype::PEM,
    )?;
  }
  if let Some(root_cert) = params.get("sslrootcert") {
    tls_builder.set_ca_file(root_cert)?;
  }
  if params.get("sslmode")
    == Some(&"verify-full".to_string())
  {
    tls_builder
      .set_verify(openssl::ssl::SslVerifyMode::PEER);
  }
  Ok(postgres_openssl::MakeTlsConnector::new(
    tls_builder.build(),
  ))
}

fn get_config(
  url: &url::Url,
  params: &HashMap<String, String>,
) -> DynResult<tokio_postgres::config::Config> {
  let mut config = tokio_postgres::config::Config::new();
  if url.username() != "" {
    config.user(url.username());
  }
  if let Some(password) = url.password() {
    let decoded_passowrd: &str =
      &percent_encoding::percent_decode_str(password)
        .decode_utf8()?;
    config.password(decoded_passowrd);
  }
  if let Some(host) = url.host_str() {
    config.host(host);
  }
  if let Some(port) = url.port() {
    config.port(port);
  }
  if let Some(mut path_segments) = url.path_segments() {
    if let Some(db_name) = path_segments.next() {
      config.dbname(db_name);
    }
  }
  if let Some(options) = params.get("options") {
    config.options(options);
  }
  Ok(config)
}

pub async fn find_recent_updates(
  db_context: &Context,
  buffer_seconds: f64,
  language: Language,
  mut updated_at: Option<OffsetDateTime>,
) -> DynResult<Vec<Document>> {
  const QUERY_TEXT: &str = include_str!(
    "../../../db/src/query/recent_updates_find.sql"
  );
  const QUERY_LIMIT: i64 = 256;
  let mut done = false;
  let mut type_: Option<String> = None;
  let mut id: Option<Vec<u8>> = None;
  let mut results = Vec::<Document>::new();
  while !done {
    let rows = &db_context
      .client
      .query(
        QUERY_TEXT,
        &[
          &buffer_seconds,
          &QUERY_LIMIT,
          &language,
          &updated_at,
          &type_,
          &id,
        ],
      )
      .await?;
    done = rows.len() < QUERY_LIMIT as usize;
    results.append(
      &mut rows
        .iter()
        .map(create_document)
        .collect::<DynResult<Vec<Document>>>()?,
    );
    if !done {
      let last = results.last().ok_or("no last")?;
      updated_at = Some(last.updated_at);
      type_ = Some(last.type_.clone());
      id = Some(last.id.clone());
    }
  }
  Ok(results)
}

fn create_document(
  row: &tokio_postgres::Row,
) -> DynResult<Document> {
  Ok(Document {
    type_: row.try_get("type")?,
    id: row.try_get("id")?,
    created_at: row.try_get("created_at")?,
    created_by: row.try_get("created_by")?,
    updated_at: row.try_get("updated_at")?,
    deleted: row.try_get("deleted")?,
    subforum_id: row.try_get("subforum_id")?,
    topic_id: row.try_get("topic_id")?,
    taxon_rank: row.try_get("taxon_rank")?,
    parents: from_option_json_array(
      row.try_get("parents")?,
      Vec::from_hex,
    )?,
    title: row.try_get("title")?,
    names: from_json_array(
      row.try_get("names")?,
      |item| Ok(item.to_owned()),
    )?,
    tags: from_json_array(
      row.try_get("tags")?,
      Vec::from_hex,
    )?,
    content: row.try_get("content")?,
  })
}

fn from_json_array<B, F>(
  value: serde_json::value::Value,
  mut f: F,
) -> DynResult<Vec<B>>
where
  F: FnMut(&str) -> DynResult<B>,
{
  value
    .as_array()
    .ok_or("not an array")?
    .iter()
    .map(move |item| f(item.as_str().ok_or("not a str")?))
    .collect::<DynResult<Vec<B>>>()
}

fn from_option_json_array<B, F>(
  value: Option<serde_json::value::Value>,
  f: F,
) -> DynResult<Option<Vec<B>>>
where
  F: FnMut(&str) -> DynResult<B>,
{
  if let Some(value) = value {
    Ok(Some(from_json_array(value, f)?))
  } else {
    Ok(None)
  }
}
