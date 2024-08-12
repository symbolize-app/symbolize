use crate::db as svc_db;
use crate::random as svc_random;
use crate::state as svc_state;

#[derive(Debug)]
pub struct ContextImpl {
  pub db: svc_db::DbImpl,
  pub random: svc_random::RandomImpl,
  pub state: svc_state::StateImpl,
}

impl ContextImpl {
  pub async fn wait(&self) {
    self.db.wait().await;
  }
}

impl svc_db::Context for ContextImpl {
  type Impl = svc_db::DbImpl;

  fn db(&self) -> &Self::Impl {
    &self.db
  }
}

impl svc_random::Context for ContextImpl {
  type Impl = svc_random::RandomImpl;

  fn random(&self) -> &Self::Impl {
    &self.random
  }
}

impl svc_state::Context for ContextImpl {
  type Impl = svc_state::StateImpl;

  fn state(&self) -> &Self::Impl {
    &self.state
  }
}
