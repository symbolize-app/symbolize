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
