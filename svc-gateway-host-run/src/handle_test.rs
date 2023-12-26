use super::*;
use crate::context as svc_context;
use crate::db as svc_db;
use crate::request as svc_request;
use crate::response as svc_response;
use mockall::predicate::eq;

#[tokio::test]
async fn test_handle_content_by_id() {
  let mut db = svc_db::MockContext::new();
  db.expect_get_content_by_id()
    .with(eq(vec![0x01, 0x0f]))
    .times(1)
    .returning(|_| Ok(Some(vec![9])));

  let mut ctx = svc_context::MockDbContext::new();
  ctx.expect_db().times(1).return_const(db);

  let req = svc_request::ContentRequest {
    full_path: "010f.js",
    path_prefix: "010f",
    mime: svc_request::ContentMime::JavaScript,
    sandbox: false,
  };

  assert_eq!(
    handle_content_by_id(&ctx, &req).await.unwrap(),
    Some(svc_response::ContentResponse {
      sandbox: false,
      mime: svc_request::ContentMime::JavaScript,
      is_service_worker_shell: false,
      body: vec![9].into()
    })
  );
}

#[tokio::test]
async fn test_handle_content_by_path() {
  let mut db = svc_db::MockContext::new();
  db.expect_get_content_by_path()
    .with(eq("a.js".to_owned()))
    .times(1)
    .returning(|_| Ok(Some(vec![8])));

  let mut ctx = svc_context::MockDbContext::new();
  ctx.expect_db().times(1).return_const(db);

  let req = svc_request::ContentRequest {
    full_path: "a.js",
    path_prefix: "a",
    mime: svc_request::ContentMime::JavaScript,
    sandbox: false,
  };

  assert_eq!(
    handle_content_by_path(&ctx, &req).await.unwrap(),
    Some(svc_response::ContentResponse {
      sandbox: false,
      mime: svc_request::ContentMime::JavaScript,
      is_service_worker_shell: false,
      body: vec![8].into()
    })
  );
}
