use anyhow::Error;
use anyhow::Result;
use bytes::Bytes;
use http_body_util::Empty;
use hyper::client::conn::http2::SendRequest;
use rustyline_async::SharedWriter;
use tokio::sync::watch;
use tokio::task::JoinHandle;

#[allow(missing_debug_implementations)]
pub struct Context {
  pub stdout: SharedWriter,
  pub selected_connection_id: usize,
  pub connections: Vec<SimConnection>,
}

#[derive(Debug)]
pub struct SimConnection {
  pub task: JoinHandle<Result<(), Error>>,
  pub sender_tx: watch::Sender<Option<Result<SendRequest<Empty<Bytes>>>>>,
  pub sender_rx:
    watch::Receiver<Option<Result<SendRequest<Empty<Bytes>>>>>,
  pub selected_stream_id: usize,
  pub streams: Vec<SimStream>,
}

#[derive(Debug)]
pub struct SimStream {
  pub task: JoinHandle<Result<(), Error>>,
}
