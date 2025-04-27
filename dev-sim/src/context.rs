use rustyline_async::SharedWriter;

#[allow(missing_debug_implementations)]
#[derive(Clone)]
pub struct Context {
  pub stdout: SharedWriter,
}
