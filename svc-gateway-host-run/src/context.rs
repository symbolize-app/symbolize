use crate::db as svc_db;

pub struct ContextImpl {
  pub db: svc_db::DbImpl,
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
