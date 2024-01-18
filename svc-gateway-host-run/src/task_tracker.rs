use hyper::rt::Executor as HyperExecutor;
use std::future::Future;
use tokio_util::task::TaskTracker;

#[derive(Clone)]
pub struct TaskTrackerExecutor {
  pub task_tracker: TaskTracker,
}

impl TaskTrackerExecutor {
  pub fn new(task_tracker: &TaskTracker) -> Self {
    TaskTrackerExecutor {
      task_tracker: task_tracker.clone(),
    }
  }
}

impl<F> HyperExecutor<F> for TaskTrackerExecutor
where
  F: Future + Send + 'static,
  F::Output: Send + 'static,
{
  fn execute(&self, future: F) {
    self.task_tracker.spawn(future);
  }
}
