use crate::task_tracker_ext::TaskTrackerExt as _;
use anyhow::anyhow;
use anyhow::Error;
use anyhow::Result;
use crossbeam::queue::ArrayQueue;
#[cfg(test)]
use mockall::automock;
#[cfg(test)]
use mockall::mock;
use rusqlite;
use rusqlite::OptionalExtension as _;
use std::future::Future;
use std::sync::Arc;
use std::thread;
use std::thread::ThreadId;
use std::time::Duration;
use tokio::process::Command;
use tokio::runtime::Handle as TokioHandle;
use tokio::select;
use tokio::sync::oneshot;
use tokio::time::sleep;
use tokio_util::task::TaskTracker;

const MAX_IDLE_CONNECTIONS: usize = 512;
const IDLE_CONNECTION_TIMEOUT: Duration = Duration::from_secs(30);

pub trait Context
where
  Self::Impl: Db,
{
  type Impl;

  fn db(&self) -> &Self::Impl;
}

#[cfg(test)]
mock! {
  pub Context {}
  impl Context for Context {
      type Impl = MockDb;
      fn db(&self) -> &<Self as Context>::Impl;
  }
}

#[cfg_attr(test, automock)]
pub trait Db {
  fn get_content_by_id(
    &self,
    content_id: Vec<u8>,
  ) -> impl Future<Output = Result<Option<Vec<u8>>>> + Send;

  fn get_content_by_path(
    &self,
    path_id: String,
  ) -> impl Future<Output = Result<Option<ContentRowWithId>>> + Send;
}

#[derive(Debug)]
pub struct DbImpl {
  connection_task_tracker: TaskTracker,
  idle_connections:
    Arc<ArrayQueue<(ThreadId, oneshot::Sender<QueryRequest>)>>,
}

struct MainQueryContext<'connection> {
  get_content_by_id: rusqlite::Statement<'connection>,
  get_content_by_path: rusqlite::Statement<'connection>,
}

#[derive(Debug)]
pub struct ContentRowWithId {
  pub id: Vec<u8>,
  pub original: Vec<u8>,
}

trait QueryFunction = for<'a> FnOnce(
  Result<(&'a rusqlite::Connection, &mut MainQueryContext<'a>)>,
) -> Result<()>;

struct QueryRequest(Box<dyn QueryFunction + Send>);

impl DbImpl {
  pub async fn init() -> Result<Self> {
    Self::run_migrations().await?;
    Ok(DbImpl {
      connection_task_tracker: TaskTracker::new(),
      idle_connections: Arc::new(ArrayQueue::new(MAX_IDLE_CONNECTIONS)),
    })
  }

  pub async fn wait(&self) {
    // Drop all TXs, will raise RX errors
    while self.idle_connections.pop().is_some() {}
    self.connection_task_tracker.close();
    self.connection_task_tracker.wait().await;
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

  #[allow(clippy::print_stdout)]
  #[allow(clippy::print_stderr)]
  async fn run_query<T, F>(&self, func: F) -> Result<T>
  where
    T: Send + 'static,
    F: for<'a> FnOnce(
      &'a rusqlite::Connection,
      &mut MainQueryContext<'a>,
    ) -> Result<T>,
    F: Send + 'static,
  {
    let (response_tx, response_rx) = oneshot::channel();

    let mut request = QueryRequest(Box::new(|context| {
      context.and_then(|(connection, query)| {
        let result = func(connection, query);
        response_tx.send(result).map_err(|_| anyhow!("send error"))
      })
    }));

    while let Some((thread_id, idle_request_tx)) =
      self.idle_connections.pop()
    {
      match idle_request_tx.send(request) {
        Ok(()) => {
          println!("  DB old thread {thread_id:?}");
          return response_rx.await?;
        }
        Err(send_failed) => {
          eprintln!("  DB thread closed {thread_id:?}");
          request = send_failed;
        }
      }
    }

    let handle = TokioHandle::current();
    let idle_connections = self.idle_connections.clone();
    self.connection_task_tracker.spawn_thread(move || {
      let thread_id = thread::current().id();
      println!("  DB new thread {thread_id:?}");

      if let Err(err) =
        Self::run_thread(&handle, &idle_connections, thread_id, request)
      {
        eprintln!("  DB thread {thread_id:?} error: {err:?}");
      }
    });

    response_rx.await?
  }

  #[allow(clippy::print_stderr)]
  #[allow(clippy::print_stdout)]
  fn run_thread(
    handle: &TokioHandle,
    idle_connections: &ArrayQueue<(
      ThreadId,
      oneshot::Sender<QueryRequest>,
    )>,
    thread_id: ThreadId,
    init_request: QueryRequest,
  ) -> Result<()> {
    let connection = match Self::open_db_connection() {
      Ok(connection) => connection,
      Err(err) => {
        init_request.err(err);
        return Err(anyhow!("open DB error"));
      }
    };
    let mut query = match Self::prepare_statements(&connection) {
      Ok(query) => query,
      Err(err) => {
        init_request.err(err);
        return Err(anyhow!("prepare statements error"));
      }
    };
    init_request.query(&connection, &mut query)?;

    loop {
      let (idle_request_tx, mut idle_request_rx) = oneshot::channel();

      // Drop any old/replaced TX if queue full, will raise an RX error
      let replaced =
        idle_connections.force_push((thread_id, idle_request_tx));
      if let Some((other_thread_id, _)) = replaced {
        println!("Replaced DB thread {other_thread_id:?}");
      }

      let idle_request_result = handle.block_on(async {
        select! {
          idle_request = (&mut idle_request_rx) =>
            idle_request.map_err(
              |_| {
                // Only error possible is TX closed
                oneshot::error::TryRecvError::Closed
              }
            ),
          () = sleep(IDLE_CONNECTION_TIMEOUT) => {
            // Trigger an error for future TX, but stil check for raced TX
            println!("Closing DB thread {thread_id:?}");
            idle_request_rx.close();
            idle_request_rx.try_recv()
          }
        }
      });

      match idle_request_result {
        Err(oneshot::error::TryRecvError::Closed) => {
          break;
        }
        idle_request_result => {
          idle_request_result?.query(&connection, &mut query)?;
        }
      }
    }

    Ok(())
  }
}

impl QueryRequest {
  #[allow(clippy::print_stderr)]
  fn err(self, err: Error) {
    if let Err(err) = (self.0)(Err(err)) {
      eprintln!("Send error: {err:?}");
    }
  }

  fn query<'a>(
    self,
    connection: &'a rusqlite::Connection,
    query: &mut MainQueryContext<'a>,
  ) -> Result<()> {
    (self.0)(Ok((connection, query)))
  }
}

impl Db for DbImpl {
  async fn get_content_by_id(
    &self,
    content_id: Vec<u8>,
  ) -> Result<Option<Vec<u8>>> {
    self
      .run_query(move |_, query| {
        Ok(
          query
            .get_content_by_id
            .query_row(
              rusqlite::named_params! {":content_id": content_id},
              |row| row.get("original"),
            )
            .optional()?,
        )
      })
      .await
  }

  async fn get_content_by_path(
    &self,
    path_id: String,
  ) -> Result<Option<ContentRowWithId>> {
    self
      .run_query(move |_, query| {
        Ok(
          query
            .get_content_by_path
            .query_row(
              rusqlite::named_params! {":path_id": path_id},
              |row| {
                Ok(ContentRowWithId {
                  id: row.get("id")?,
                  original: row.get("original")?,
                })
              },
            )
            .optional()?,
        )
      })
      .await
  }
}
