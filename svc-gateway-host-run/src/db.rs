use anyhow::anyhow;
use anyhow::Result;
use rusqlite;
use self_cell::self_cell;
use tokio::process::Command;

self_cell!(
  pub struct Context {
    owner: rusqlite::Connection,

    #[covariant]
    dependent: QueryContext,
  }
);

pub struct QueryContext<'connection> {
  pub get_content_by_id: rusqlite::Statement<'connection>,
  pub get_content_by_path: rusqlite::Statement<'connection>,
}

// Assumed safe since owner and dependent are sent together
unsafe impl Send for Context {}

pub async fn init_db_context() -> Result<Context> {
  run_migrations().await?;
  let connection = open_db_connection()?;
  Context::try_new(connection, prepare_statements)
}

async fn run_migrations() -> Result<()> {
  let success = Command::new("dbmate")
    .args(["--no-dump-schema", "up"])
    .status()
    .await?
    .success();
  success.then_some(()).ok_or(anyhow!("dbmate failed"))
}

fn open_db_connection() -> Result<rusqlite::Connection> {
  Ok(rusqlite::Connection::open_with_flags(
    "svc-gateway-host-store/build/manifest.sqlite3",
    rusqlite::OpenFlags::SQLITE_OPEN_READ_ONLY
      | rusqlite::OpenFlags::SQLITE_OPEN_NO_MUTEX,
  )?)
}

fn prepare_statements(
  connection: &rusqlite::Connection,
) -> Result<QueryContext<'_>> {
  Ok(QueryContext {
    get_content_by_id: connection
      .prepare(include_str!("query/get_content_by_id.sql"))?,
    get_content_by_path: connection
      .prepare(include_str!("query/get_content_by_path.sql"))?,
  })
}
