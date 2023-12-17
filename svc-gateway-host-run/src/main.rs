use anyhow::anyhow;
use anyhow::Ok as AnyOk;
use anyhow::Result;
use headers::ContentType;
use headers::Error as HeadersError;
use headers::Header;
use headers::HeaderMapExt as _;
use http_body_util::Full as FullBody;
use hyper::body::Bytes as BodyBytes;
use hyper::body::Incoming as IncomingBody;
use hyper::header::HeaderName;
use hyper::header::HeaderValue;
use hyper::server::conn::http1;
use hyper::service::service_fn;
use hyper::Request;
use hyper::Response;
use hyper_util::rt::TokioIo;
use mime::Mime;
use mime::TEXT_HTML;
use mime::TEXT_JAVASCRIPT;
use rusqlite::named_params;
use rusqlite::Connection as DbConnection;
use rusqlite::OpenFlags as DbOpenFlags;
use rusqlite::Statement as DbStatement;
use self_cell::self_cell;
use std::env;
use std::iter::once;
use std::net::SocketAddr;
use std::sync::Arc;
use std::sync::Mutex;
use tokio::net::TcpListener;
use tokio::process::Command;
use tokio::task::spawn;
use tokio::task::spawn_blocking;

#[allow(clippy::print_stdout)]
#[tokio::main]
async fn main() -> Result<()> {
  run_migrations().await?;
  println!("Migrated");

  let db = Arc::new(Mutex::new(init_db_context().await?));
  let ctx = Context { db };
  let service = service_fn(move |req| handle(ctx.clone(), req));
  println!("Opened");

  let port: u16 = env::var("APP_DEV_PORT")?.parse()?;
  let addr = SocketAddr::from(([127, 0, 0, 1], port));
  let listener = TcpListener::bind(addr).await?;
  println!("Serving at http://{addr:}");

  loop {
    let (stream, _) = listener.accept().await?;
    let io = TokioIo::new(stream);
    let service = service.clone();

    spawn(async move {
      if let Err(err) =
        http1::Builder::new().serve_connection(io, service).await
      {
        println!("Error serving connection: {err:?}");
      }
    });
  }
}

async fn init_db_context() -> Result<DbContext> {
  run_migrations().await?;
  let connection = open_db_connection()?;
  //let query = prepare_db_queries(&connection)?;
  //let query = Box::new(connection.prepare("sql")?);
  DbContext::try_new(connection, |connection| {
    prepare_statements(connection)
  })
}

async fn run_migrations() -> Result<()> {
  let success = Command::new("dbmate")
    .args(["--no-dump-schema", "up"])
    .status()
    .await?
    .success();
  success.then_some(()).ok_or(anyhow!("dbmate failed"))
}

fn open_db_connection() -> Result<DbConnection> {
  Ok(DbConnection::open_with_flags(
    "svc-gateway-host-store/build/manifest.sqlite3",
    DbOpenFlags::SQLITE_OPEN_READ_ONLY | DbOpenFlags::SQLITE_OPEN_NO_MUTEX,
  )?)
}

fn prepare_statements(
  connection: &DbConnection,
) -> Result<DbQueryContext<'_>> {
  Ok(DbQueryContext {
    get_content: connection
      .prepare(include_str!("query/get_content.sql"))?,
  })
}

#[derive(Clone)]
struct Context {
  db: Arc<Mutex<DbContext>>,
}

self_cell!(
  struct DbContext {
    owner: DbConnection,

    #[covariant]
    dependent: DbQueryContext,
  }
);

struct DbQueryContext<'connection> {
  get_content: DbStatement<'connection>,
}

// Assumed safe since owner and dependent are sent together
unsafe impl Send for DbContext {}

#[allow(clippy::print_stderr)]
async fn handle(
  ctx: Context,
  req: Request<IncomingBody>,
) -> Result<Response<FullBody<BodyBytes>>> {
  match handle_paths(ctx, req).await {
    Err(err) => {
      eprintln!("Internal error {err:?}");
      Ok(
        Response::builder()
          .status(500)
          .body(FullBody::new(BodyBytes::from("Internal error")))?,
      )
    }
    response => response,
  }
}

const ASSETS_PATH: &str = "/.assets/";
const INDEX_HTML_PATH: &str = "svc-gateway-guest-run/index.html";
const SERVICE_WORKER_PATH: &str =
  "svc-gateway-guest-run/serviceWorker.ts.js";

async fn handle_paths(
  ctx: Context,
  req: Request<IncomingBody>,
) -> Result<Response<FullBody<BodyBytes>>> {
  let path = req.uri().path();
  let response = if let Some(path) = path.strip_prefix(ASSETS_PATH) {
    handle_content(&ctx, &req, path).await?
  } else if path == "/" {
    handle_content(&ctx, &req, INDEX_HTML_PATH).await?
  } else {
    None
  };
  if let Some(response) = response {
    Ok(response)
  } else {
    Ok(
      Response::builder()
        .status(404)
        .body(FullBody::new(BodyBytes::from("Not found")))?,
    )
  }
}

async fn handle_content(
  ctx: &Context,
  _: &Request<IncomingBody>,
  path: &str,
) -> Result<Option<Response<FullBody<BodyBytes>>>> {
  let db = ctx.db.clone();
  let path_copy = path.to_owned();
  let content = spawn_blocking(move || {
    let mut db = db.lock().expect("failed to lock db");
    let content: Option<Vec<u8>> = db.with_dependent_mut(|_, query| {
      query
        .get_content
        .query_row(named_params! {":path_id": path_copy}, |row| row.get(0))
    })?;
    AnyOk(content)
  })
  .await??;
  match content {
    None => Ok(None),
    Some(content) => {
      let mut response = Response::builder();
      let headers =
        response.headers_mut().ok_or(anyhow!("response error"))?;
      headers.typed_insert(ContentType::from(get_media_type(path)?));
      if path == SERVICE_WORKER_PATH {
        headers.typed_insert(ServiceWorkerAllowed("/"));
      }
      let response =
        response.body(FullBody::new(BodyBytes::from(content)))?;
      Ok(Some(response))
    }
  }
}

fn get_media_type(path: &str) -> Result<Mime> {
  match path.rsplit_once('.') {
    Some((_, "html")) => Ok(TEXT_HTML),
    Some((_, "js" | "mjs")) => Ok(TEXT_JAVASCRIPT),
    _ => Err(anyhow!("invalid extension for {path:?}")),
  }
}

static SERVICE_WORKER_ALLOWED: HeaderName =
  HeaderName::from_static("service-worker-allowed");

struct ServiceWorkerAllowed(&'static str);

impl Header for ServiceWorkerAllowed {
  fn name() -> &'static HeaderName {
    &SERVICE_WORKER_ALLOWED
  }

  fn decode<'i, I>(_values: &mut I) -> Result<Self, HeadersError>
  where
    I: Iterator<Item = &'i HeaderValue>,
  {
    Err(HeadersError::invalid())
  }

  fn encode<E>(&self, values: &mut E)
  where
    E: Extend<HeaderValue>,
  {
    let value = HeaderValue::from_static(self.0);
    values.extend(once(value));
  }
}
