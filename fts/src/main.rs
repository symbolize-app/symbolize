mod message;

use crate::message::get_message;
use hyper::service::Service;
use hyper::Body;
use hyper::Request;
use hyper::Response;
use hyper::Server;
use std::collections::HashMap;
use std::env;
use std::error::Error;
use std::fs::create_dir_all;
use std::future::Future;
use std::net::Ipv4Addr;
use std::net::SocketAddr;
use std::path::Path;
use tower::make::Shared;
use std::pin::Pin;
use tantivy::collector::TopDocs;
use tantivy::directory::MmapDirectory;
use tantivy::doc;
use tantivy::query::QueryParser;
use tantivy::schema;
use tantivy::schema::Document;
use tantivy::schema::Field;
use tantivy::schema::Schema;
use tantivy::Index;
use tantivy::IndexReader;
use tantivy::ReloadPolicy;
use url::form_urlencoded;

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

impl Service<Request<Body>> for Context {
  type Response = Response<Body>;
  type Error = Box<dyn Error + Send + Sync>;

  #[allow(clippy::type_complexity)]
  type Future = Pin<
    Box<
      dyn Future<Output = Result<Self::Response, Self::Error>>
        + Send
        + Sync,
    >,
  >;

  fn poll_ready(
    &mut self,
    _cx: &mut std::task::Context<'_>,
  ) -> std::task::Poll<Result<(), Self::Error>> {
    std::task::Poll::Ready(Ok(()))
  }

  fn call(&mut self, req: Request<Body>) -> Self::Future {
    let context = self.clone();
    Box::pin(hello_world(context, req))
  }
}

#[tokio::main]
async fn main() -> Result<(), Box<dyn Error>> {
  let host: Ipv4Addr = env::var("FTS_HOST")?.parse()?;
  let port: u16 = env::var("FTS_PORT")?.parse()?;
  let addr = SocketAddr::from((host, port));

  let context = load_context()?;
  let server =
    Server::bind(&addr).serve(Shared::new(context));
  server.await?;

  Ok(())
}

fn load_context() -> Result<Context, Box<dyn Error>> {
  let (schema, index, index_reader, fields) = load_index()?;
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
  let title = schema_builder
    .add_text_field("title", schema::TEXT | schema::STORED);
  let body =
    schema_builder.add_text_field("body", schema::TEXT);
  let schema = schema_builder.build();
  let index =
    Index::open_or_create(index_dir, schema.clone())?;
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

#[allow(clippy::unused_async)]
async fn hello_world(
  context: Context,
  req: Request<Body>,
) -> Result<Response<Body>, Box<dyn Error + Send + Sync>> {
  let searcher = context.index_reader.searcher();
  let query_parser = QueryParser::for_index(
    &context.index,
    vec![context.fields.title, context.fields.body],
  );
  print!("{}", get_message());
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
