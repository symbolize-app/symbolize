use std::error::Error;

pub type DynResult<E> =
  Result<E, Box<dyn Error + Send + Sync>>;
