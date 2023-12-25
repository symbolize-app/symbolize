use crate::db as svc_db;
use std::sync::Arc;
use std::sync::Mutex;

#[derive(Clone)]
pub struct Context {
  pub db: Arc<Mutex<svc_db::Context>>,
}
