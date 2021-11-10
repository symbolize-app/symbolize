mod message;

use crate::message::get_message;
use hyper::service::service_fn;
use hyper::Body;
use hyper::Request;
use hyper::Response;
use hyper::Server;
use openssl::ssl::SslConnector;
use openssl::ssl::SslFiletype;
use openssl::ssl::SslMethod;
use openssl::ssl::SslVerifyMode;
use percent_encoding::percent_decode_str;
use postgres_openssl::MakeTlsConnector;
use std::collections::HashMap;
use std::env;
use std::error::Error;
use std::fs::create_dir_all;
use std::net::Ipv4Addr;
use std::net::SocketAddr;
use std::path::Path;
use tantivy::collector::TopDocs;
use tantivy::directory::MmapDirectory;
use tantivy::doc;
use tantivy::query::QueryParser;
use tantivy::schema::Document;
use tantivy::schema::Field;
use tantivy::schema::IndexRecordOption;
use tantivy::schema::Schema;
use tantivy::schema::TextFieldIndexing;
use tantivy::schema::TextOptions;
use tantivy::tokenizer::Language;
use tantivy::tokenizer::LowerCaser;
use tantivy::tokenizer::SimpleTokenizer;
use tantivy::tokenizer::Stemmer;
use tantivy::tokenizer::TextAnalyzer;
use tantivy::Index;
use tantivy::IndexReader;
use tantivy::ReloadPolicy;
use tokio_postgres::config::Config;
use tower::make::Shared;
use url::form_urlencoded;
use url::Url;

#[derive(Clone)]
struct Context {
  schema: Schema,
  index: Index,
  index_reader: IndexReader,
  fields: FieldSet,
}

#[derive(Clone)]
struct FieldSet {
  title: Field,
  body: Field,
}

#[tokio::main]
async fn main() -> Result<(), Box<dyn Error>> {
  let context = load_context().await?;
  run_server(context).await?;
  Ok(())
}

async fn load_context() -> Result<Context, Box<dyn Error>> {
  let (schema, index, index_reader, fields) = load_index()?;
  load_db().await?;
  Ok(Context {
    schema,
    index,
    index_reader,
    fields,
  })
}

fn load_index() -> Result<
  (Schema, Index, IndexReader, FieldSet),
  Box<dyn Error>,
> {
  let index_path = Path::new("./.tantivy/topic");
  create_dir_all(index_path)?;
  let index_dir = MmapDirectory::open(index_path)?;
  let mut schema_builder = Schema::builder();
  let text_indexing_options = TextFieldIndexing::default()
    .set_tokenizer("basic")
    .set_index_option(
      IndexRecordOption::WithFreqsAndPositions,
    );
  let text_options = TextOptions::default()
    .set_indexing_options(text_indexing_options)
    .set_stored();
  let title = schema_builder
    .add_text_field("title", text_options.clone());
  let body =
    schema_builder.add_text_field("body", text_options);
  let schema = schema_builder.build();
  let index =
    Index::open_or_create(index_dir, schema.clone())?;
  let tokenizer = TextAnalyzer::from(SimpleTokenizer)
    .filter(LowerCaser)
    .filter(Stemmer::new(Language::English));
  index.tokenizers().register("basic", tokenizer);
  let index_meta = index.load_metas()?;
  if index_meta.payload != Some("TEST".to_string()) {
    let mut index_writer = index.writer(50_000_000)?;
    let mut old_man_doc = Document::default();
    old_man_doc.add_text(title, "The Old Man and the Sea");
    old_man_doc.add_text(
        body,
        "He was an old man who fished alone in a skiff in the Gulf Stream and \
         he had gone eighty-four days now without taking a fish.",
    );

    // ... and add it to the `IndexWriter`.
    index_writer.add_document(old_man_doc);
    index_writer.add_document(doc!(
    title => "Of Mice and Men",
    body => "A few miles south of Soledad, the Salinas River drops in close to the hillside \
            bank and runs deep and green. The water is warm too, for it has slipped twinkling \
            over the yellow sands in the sunlight before reaching the narrow pool. On one \
            side of the river the golden foothill slopes curve up to the strong and rocky \
            Gabilan Mountains, but on the valley side the water is lined with trees—willows \
            fresh and green with every spring, carrying in their lower leaf junctures the \
            debris of the winter’s flooding; and sycamores with mottled, white, recumbent \
            limbs and branches that arch over the pool"
    ));
    index_writer.add_document(doc!(
      title => "Frankenstein",
      title => "The Modern Prometheus",
      body => "You will rejoice to hear that no disaster has accompanied the commencement of an \
               enterprise which you have regarded with such evil forebodings.  I arrived here \
               yesterday, and my first task is to assure my dear sister of my welfare and \
               increasing confidence in the success of my undertaking."
      ));
    let mut prepared_commit =
      index_writer.prepare_commit()?;
    prepared_commit.set_payload("TEST");
    prepared_commit.commit()?;
  }

  let index_reader = index
    .reader_builder()
    .reload_policy(ReloadPolicy::OnCommit)
    .try_into()?;

  Ok((
    schema,
    index,
    index_reader,
    FieldSet { title, body },
  ))
}

async fn load_db() -> Result<(), Box<dyn Error>> {
  let url: Url =
    env::var("DATABASE_URL_API_READ")?.parse()?;
  let params = url
    .query_pairs()
    .into_owned()
    .collect::<HashMap<String, String>>();
  let mut tls_builder =
    SslConnector::builder(SslMethod::tls())?;
  if let Some(cert) = params.get("sslcert") {
    tls_builder.set_certificate_chain_file(cert)?;
  }
  if let Some(key) = params.get("sslkey") {
    tls_builder
      .set_private_key_file(key, SslFiletype::PEM)?;
  }
  if let Some(root_cert) = params.get("sslrootcert") {
    tls_builder.set_ca_file(root_cert)?;
  }
  if params.get("sslmode")
    == Some(&"verify-full".to_string())
  {
    tls_builder.set_verify(SslVerifyMode::PEER);
  }
  let tls = MakeTlsConnector::new(tls_builder.build());
  let mut config = Config::new();
  if url.username() != "" {
    config.user(url.username());
  }
  if let Some(password) = url.password() {
    let decoded_passowrd: &str =
      &percent_decode_str(password).decode_utf8()?;
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
  for row in
    &client.query("SELECT title FROM topic", &[]).await?
  {
    let id: &str = row.get(0);
    println!("{}", id);
  }
  println!("DONE");
  Ok(())
}

async fn run_server(
  context: Context,
) -> Result<(), Box<dyn Error>> {
  let host: Ipv4Addr = env::var("FTS_HOST")?.parse()?;
  let port: u16 = env::var("FTS_PORT")?.parse()?;
  let addr = SocketAddr::from((host, port));

  let service = service_fn(move |req| {
    handle_request(context.clone(), req)
  });
  let server =
    Server::bind(&addr).serve(Shared::new(service));
  server.await?;
  Ok(())
}

#[allow(clippy::unused_async)]
async fn handle_request(
  context: Context,
  req: Request<Body>,
) -> Result<Response<Body>, Box<dyn Error + Send + Sync>> {
  let searcher = context.index_reader.searcher();
  let query_parser = QueryParser::for_index(
    &context.index,
    vec![context.fields.title, context.fields.body],
  );
  println!("{}", get_message());
  let query = query_parser.parse_query(
    form_urlencoded::parse(
      req.uri().query().ok_or("invalid query")?.as_bytes(),
    )
    .into_owned()
    .collect::<HashMap<String, String>>()
    .get("q")
    .ok_or("missing query param")?,
  )?;
  let top_docs =
    searcher.search(&query, &TopDocs::with_limit(10))?;
  let mut result = "".to_string();
  for (_score, doc_address) in top_docs {
    let retrieved_doc = searcher.doc(doc_address)?;
    result
      .push_str(&context.schema.to_json(&retrieved_doc));
  }
  Ok(Response::new(result.into()))
}
