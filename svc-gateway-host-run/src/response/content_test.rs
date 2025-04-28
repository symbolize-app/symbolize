use super::*;
use crate::header as svc_header;
use crate::header::HeaderPair;

#[tokio::test]
async fn test_no_sandbox_content_security_policy() {
  let response = ContentResponse {
    sandbox: false,
    mime: svc_request::ContentMime::JavaScript,
    is_service_worker_shell: false,
    immutable: true,
    e_tag: "010f".to_owned(),
    original: vec![0x09],
  };
  let response = response.into_response().unwrap();
  let content_security_policy = response
    .headers()
    .get(svc_header::ContentSecurityPolicy::key())
    .unwrap();
  assert_eq!(
    content_security_policy.to_str().unwrap(),
    include_str!(
      "../../../svc-gateway-guest-run/contentSecurityPolicy.txt"
    )
    .trim_end()
  );
}
