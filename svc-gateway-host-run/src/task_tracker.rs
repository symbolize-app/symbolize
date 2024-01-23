use hyper::rt::Executor as HyperExecutor;
use std::future::Future;
use std::thread;
use std::thread::JoinHandle as StdJoinHandle;
use tokio_util::task::TaskTracker;

pub trait TaskTrackerExt {
  fn spawn_thread<F, T>(&self, func: F) -> StdJoinHandle<T>
  where
    F: FnOnce() -> T,
    F: Send + 'static,
    T: Send + 'static;
}

impl TaskTrackerExt for TaskTracker {
  fn spawn_thread<F, T>(&self, func: F) -> StdJoinHandle<T>
  where
    F: FnOnce() -> T,
    F: Send + 'static,
    T: Send + 'static,
  {
    let token = self.token();
    thread::spawn(move || {
      let result = func();
      drop(token);
      result
    })
  }
}

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
