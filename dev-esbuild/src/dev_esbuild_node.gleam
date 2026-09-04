import lib_error

@external(javascript, "./node_ffi.mjs", "argv")
pub fn argv() -> List(String)

@external(javascript, "./node_ffi.mjs", "resolve")
pub fn resolve(path: String) -> String

@external(javascript, "./node_ffi.mjs", "relative")
pub fn relative(from: String, to: String) -> String

@external(javascript, "./node_ffi.mjs", "now_milliseconds")
pub fn now_milliseconds() -> Int

@external(javascript, "./node_ffi.mjs", "remove")
fn remove_ffi(path: String, done: fn(Result(Nil, String)) -> Nil) -> Nil

pub fn remove(path: String) -> lib_error.Async(Nil, String) {
  fn(done) { remove_ffi(path, done) }
}

@external(javascript, "./node_ffi.mjs", "mkdir")
fn mkdir_ffi(path: String, done: fn(Result(Nil, String)) -> Nil) -> Nil

pub fn mkdir(path: String) -> lib_error.Async(Nil, String) {
  fn(done) { mkdir_ffi(path, done) }
}

@external(javascript, "./node_ffi.mjs", "temporary_directory")
pub fn temporary_directory(prefix: String) -> String

@external(javascript, "./node_ffi.mjs", "write_file")
pub fn write_file(path: String, contents: BitArray) -> Nil

@external(javascript, "./node_ffi.mjs", "read_file")
pub fn read_file(path: String) -> String

type Environment

@external(javascript, "./node_ffi.mjs", "new_environment")
fn new_environment() -> Environment

@external(javascript, "./node_ffi.mjs", "set_environment")
fn set_environment(environment: Environment, name: String, value: String) -> Nil

@external(javascript, "./node_ffi.mjs", "exec_file")
fn exec_file_ffi(
  command: String,
  arguments: List(String),
  environment: Environment,
  done: fn(Result(Nil, String)) -> Nil,
) -> Nil

pub fn exec_file(
  command: String,
  arguments: List(String),
  values: List(#(String, String)),
) -> lib_error.Async(Nil, String) {
  fn(done) {
    let environment = new_environment()
    set_environment_values(environment, values)
    exec_file_ffi(command, arguments, environment, done)
  }
}

fn set_environment_values(
  environment: Environment,
  values: List(#(String, String)),
) -> Nil {
  case values {
    [] -> Nil
    [#(name, value), ..rest] -> {
      set_environment(environment, name, value)
      set_environment_values(environment, rest)
    }
  }
}

@external(javascript, "./node_ffi.mjs", "set_failure")
pub fn set_failure() -> Nil
