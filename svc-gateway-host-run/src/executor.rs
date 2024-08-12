use crate::context as svc_context;
use crate::state::Context as _;
use crate::state::StateExt;
use hyper::rt::Executor as HyperExecutor;
use std::future::Future;
use std::sync::Arc;
use tokio_util::task::TaskTracker;

#[derive(Clone, Debug)]
pub struct Executor {
  pub ctx: Arc<svc_context::ContextImpl>,
  pub task_tracker: TaskTracker,
}

impl Executor {
  #[must_use]
  pub fn new(
    ctx: Arc<svc_context::ContextImpl>,
    task_tracker: &TaskTracker,
  ) -> Self {
    Executor {
      ctx,
      task_tracker: task_tracker.clone(),
    }
  }
}

impl<F> HyperExecutor<F> for Executor
where
  F: Future + Send + 'static,
  F::Output: Send + 'static,
{
  fn execute(&self, future: F) {
    let ctx = self.ctx.clone();
    self.task_tracker.spawn(async move {
      ctx.state().scope_response_stream(future).await;
    });
  }
}
