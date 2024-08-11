use std::thread;
use std::thread::JoinHandle as StdJoinHandle;
use tokio_util::task::task_tracker::TaskTrackerToken;
use tokio_util::task::TaskTracker;

trait TaskTrackerStruct {
  fn token_(&self) -> TaskTrackerToken;
}

impl TaskTrackerStruct for TaskTracker {
  fn token_(&self) -> TaskTrackerToken {
    self.token()
  }
}

#[allow(private_bounds)]
pub trait TaskTrackerExt: TaskTrackerStruct {
  fn spawn_thread<F, T>(&self, func: F) -> StdJoinHandle<T>
  where
    F: FnOnce() -> T,
    F: Send + 'static,
    T: Send + 'static,
  {
    let token = self.token_();
    thread::spawn(move || {
      let result = func();
      drop(token);
      result
    })
  }
}

impl TaskTrackerExt for TaskTracker {}
