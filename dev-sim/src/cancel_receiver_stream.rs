use futures::FutureExt;
use futures::Stream;
use pin_project_lite::pin_project;
use std::pin::Pin;
use std::task;
use std::task::Poll;
use tokio;
use tokio::sync::mpsc;
use tokio_util::sync::CancellationToken;
use tokio_util::sync::WaitForCancellationFutureOwned;

pin_project! {
  #[project(!Unpin)]
  pub struct CancelReceiverStream<T> {
      #[pin]
      wait_for_cancellation: WaitForCancellationFutureOwned,
      #[pin]
      receiver: mpsc::Receiver<T>,
  }
}

impl<T> CancelReceiverStream<T> {
  #[must_use]
  pub fn new(
    cancellation_token: CancellationToken,
    receiver: mpsc::Receiver<T>,
  ) -> CancelReceiverStream<T> {
    CancelReceiverStream {
      wait_for_cancellation: cancellation_token.cancelled_owned(),
      receiver,
    }
  }
}

impl<T> Stream for CancelReceiverStream<T> {
  type Item = T;

  fn poll_next(
    self: Pin<&mut Self>,
    cx: &mut task::Context<'_>,
  ) -> Poll<Option<Self::Item>> {
    let mut this = self.project();
    if !this.receiver.is_closed() {
      if let Poll::Ready(()) = this.wait_for_cancellation.poll_unpin(cx) {
        this.receiver.close();
      }
    }
    this.receiver.poll_recv(cx)
  }
}
