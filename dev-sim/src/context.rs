use anyhow::Error;
use anyhow::Result;
use bytes::Bytes;
use http_body::Body;
use hyper::body::Incoming as IncomingBody;
use hyper::Request;
use hyper::Response;
use rustyline_async::SharedWriter;
use std::pin::Pin;
use tokio::sync::mpsc;
use tokio::sync::oneshot;
use tokio::task::JoinHandle;
use tokio_util::sync::CancellationToken;

#[allow(missing_debug_implementations)]
pub struct Context {
  pub stdout: SharedWriter,
  pub selected_connection_id: usize,
  pub connections: Vec<SimConnection>,
}

pub type SimBody =
  Pin<Box<dyn Body<Data = Bytes, Error = Error> + Sync + Send>>;

pub type SimRequest =
  (Request<SimBody>, oneshot::Sender<Response<IncomingBody>>);

#[derive(Debug)]
pub struct SimConnection {
  pub task: JoinHandle<Result<(), Error>>,
  pub cancellation_token: CancellationToken,
  pub send_request_tx: mpsc::Sender<SimRequest>,
  pub selected_stream_id: usize,
  pub streams: Vec<SimStream>,
}

#[derive(Debug)]
pub struct SimStream {
  pub task: JoinHandle<Result<(), Error>>,
  pub cancellation_token: CancellationToken,
  pub send_message_tx: mpsc::Sender<String>,
}
