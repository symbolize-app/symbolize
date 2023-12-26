use anyhow::anyhow;
use anyhow::Result;
#[cfg(test)]
use mockall::automock;
use rusqlite;
use self_cell::self_cell;
use std::sync::Arc;
use std::sync::Mutex;
use tokio::process::Command;
use tokio::task::spawn_blocking;

#[cfg_attr(test, automock)]
pub trait Context {
  async fn get_content_by_id(
    &self,
    content_id: Vec<u8>,
  ) -> Result<Option<Vec<u8>>>;

  async fn get_content_by_path(
    &self,
    path_id: String,
  ) -> Result<Option<Vec<u8>>>;
}

#[derive(Clone)]
pub struct MainContext(pub Arc<Mutex<MainContextCell>>);

self_cell!(
  pub struct MainContextCell {
    owner: rusqlite::Connection,

    #[covariant]
    dependent: MainQueryContext,
  }
);

pub struct MainQueryContext<'connection> {
  pub get_content_by_id: rusqlite::Statement<'connection>,
  pub get_content_by_path: rusqlite::Statement<'connection>,
}

// Assumed safe since owner and dependent are sent together
unsafe impl Send for MainContextCell {}

impl MainContext {
  pub async fn init() -> Result<Self> {
    Self::run_migrations().await?;
    let connection = Self::open_db_connection()?;
    let cell =
      MainContextCell::try_new(connection, Self::prepare_statements)?;
    Ok(MainContext(Arc::new(Mutex::new(cell))))
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
  ) -> Result<MainQueryContext<'_>> {
    Ok(MainQueryContext {
      get_content_by_id: connection
        .prepare(include_str!("query/get_content_by_id.sql"))?,
      get_content_by_path: connection
        .prepare(include_str!("query/get_content_by_path.sql"))?,
    })
  }

  async fn spawn_blocking<T, F>(&self, func: F) -> Result<T>
  where
    T: Send + 'static,
    F: for<'a> FnOnce(
        &'a rusqlite::Connection,
        &mut MainQueryContext<'a>,
      ) -> T
      + Send
      + 'static,
  {
    let cell = self.0.clone();
    Ok(
      spawn_blocking(move || {
        let mut db = cell.lock().expect("failed to lock db");
        db.with_dependent_mut(func)
      })
      .await?,
    )
  }
}

impl Context for MainContext {
  async fn get_content_by_id(
    &self,
    content_id: Vec<u8>,
  ) -> Result<Option<Vec<u8>>> {
    Ok(
      self
        .spawn_blocking(move |_, query| {
          query.get_content_by_id.query_row(
            rusqlite::named_params! {":content_id": content_id},
            |row| row.get(0),
          )
        })
        .await??,
    )
  }

  async fn get_content_by_path(
    &self,
    path_id: String,
  ) -> Result<Option<Vec<u8>>> {
    Ok(
      self
        .spawn_blocking(move |_, query| {
          query.get_content_by_path.query_row(
            rusqlite::named_params! {":path_id": path_id},
            |row| row.get(0),
          )
        })
        .await??,
    )
  }
}
