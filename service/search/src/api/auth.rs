pub struct Auth {
  header_value: String,
}

impl Auth {
  pub fn new(username: &str, password: &str) -> Self {
    Self {
      header_value: format!(
        "Basic {}",
        base64::encode(
          format!("{}:{}", username, password).as_bytes()
        )
      ),
    }
  }

  pub fn check_request(
    &self,
    req: &hyper::Request<hyper::Body>,
  ) -> Result<(), hyper::Response<hyper::Body>> {
    match req.headers().get(hyper::header::AUTHORIZATION) {
      None => {
        let mut res =
          hyper::Response::new("unauthorized".into());
        *res.status_mut() = hyper::StatusCode::UNAUTHORIZED;
        Err(res)
      }
      Some(header_value)
        if header_value.as_bytes()
          == self.header_value.as_bytes() =>
      {
        Ok(())
      }
      _ => {
        let mut res =
          hyper::Response::new("forbidden".into());
        *res.status_mut() = hyper::StatusCode::FORBIDDEN;
        Err(res)
      }
    }
  }
}
