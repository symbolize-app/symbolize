use crate::core::document::Language;
use std::collections::HashMap;
use std::env;
use std::error::Error as StdError;

pub struct Context {
  pub client: tokio_postgres::Client,
}

pub async fn load() -> Result<Context, Box<dyn StdError>> {
  let url: url::Url =
    env::var("DATABASE_URL_API_READ")?.parse()?;
  let params = url
    .query_pairs()
    .into_owned()
    .collect::<HashMap<String, String>>();
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
  let tls = postgres_openssl::MakeTlsConnector::new(
    tls_builder.build(),
  );
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
  let (client, connection) = config.connect(tls).await?;
  tokio::spawn(async move {
    connection.await.unwrap();
    println!("CONNECTED");
  });
  const QUERY_TEXT: &str = include_str!(
    "../../../db/src/query/recent_updates_get.sql"
  );
  let updated_at: Option<std::time::SystemTime> = None;
  let typ: Option<String> = None;
  let id: Option<Vec<u8>> = None;
  for row in &client
    .query(QUERY_TEXT, &[&updated_at, &typ, &id])
    .await?
  {
    let id: Language = row.try_get(0)?;
    println!("{:?}", id);
  }
  println!("DONE");
  Ok(Context { client })
}
