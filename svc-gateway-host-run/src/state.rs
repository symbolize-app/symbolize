use anyhow::anyhow;
use anyhow::Result;
use dashmap::DashMap;
use futures::Future;
#[cfg(test)]
use mockall::automock;
#[cfg(test)]
use mockall::mock;
use std::cell::OnceCell;
use std::pin::pin;
use tokio::sync::mpsc;

pub trait Context
where
  Self::Impl: State,
{
  type Impl;

  fn state(&self) -> &Self::Impl;
}

#[cfg(test)]
mock! {
  pub Context {}
  impl Context for Context {
      type Impl =  MockState;
      fn state(&self) -> &<Self as Context>::Impl;
  }
}

tokio::task_local! {
  static RESPONSE_STREAM_ID_CELL: OnceCell<Vec<u8>>;
}

#[cfg_attr(test, automock)]
pub trait State {
  fn response_streams(&self) -> &DashMap<Vec<u8>, mpsc::Sender<Vec<u8>>>;
}

pub struct StateImpl {
  response_streams: DashMap<Vec<u8>, mpsc::Sender<Vec<u8>>>,
}

impl StateImpl {
  pub fn init() -> Self {
    StateImpl {
      response_streams: DashMap::new(),
    }
  }
}

impl State for StateImpl {
  fn response_streams(&self) -> &DashMap<Vec<u8>, mpsc::Sender<Vec<u8>>> {
    &self.response_streams
  }
}

pub trait StateExt: State {
  async fn scope_response_stream<F>(&self, f: F) -> <F as Future>::Output
  where
    F: Future,
  {
    let mut task_local_future =
      pin!(RESPONSE_STREAM_ID_CELL.scope(OnceCell::new(), f));
    let result = task_local_future.as_mut().await;
    let response_stream_id_cell = task_local_future
      .as_mut()
      .take_value()
      .expect("taken twice");
    if let Some(response_stream_id) = response_stream_id_cell.get() {
      let removed = self.response_streams().remove(response_stream_id);
      assert!(removed.is_some(), "missing response stream key");
    }
    result
  }

  fn register_response_stream(
    &self,
    response_stream_id: &[u8],
    response_stream_sender: &mpsc::Sender<Vec<u8>>,
  ) -> Result<()> {
    if self.response_streams().contains_key(response_stream_id) {
      Err(anyhow!("duplicate response stream key"))
    } else {
      self.response_streams().insert(
        response_stream_id.to_vec(),
        response_stream_sender.clone(),
      );
      RESPONSE_STREAM_ID_CELL.with(|response_stream_id_cell| {
        response_stream_id_cell
          .set(response_stream_id.to_vec())
          .expect("response stream ID cell already set");
      });
      Ok(())
    }
  }

  fn find_response_stream(
    &self,
    response_stream_id: &[u8],
  ) -> Result<mpsc::Sender<Vec<u8>>> {
    Ok(
      self
        .response_streams()
        .get(response_stream_id)
        .ok_or_else(|| anyhow!("response stream not found"))?
        .value()
        .clone(),
    )
  }
}

impl<T> StateExt for T where T: State {}
