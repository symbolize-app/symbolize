import dev_esbuild_node as node
import gleam/option.{type Option, None, Some}
import gleam/string
import lib_error

pub opaque type Database {
  Database(
    raw: RawDatabase,
    begin_transaction: Statement,
    commit_transaction: Statement,
    insert_path: Statement,
    insert_version: Statement,
    pragma_wal_checkpoint: String,
    rollback_transaction: Statement,
    update_content_compressed: Statement,
    upsert_content: Statement,
  )
}

type RawDatabase

type Statement

type Parameters

const manifest_path = "../svc-gateway-host-store/build/manifest.sqlite3"

const migrations_path = "../svc-gateway-host-store/migrate"

const schema_path = "../svc-gateway-host-store/schema.sql"

const query_path = "query"

@external(javascript, "./database_ffi.mjs", "open")
fn open_ffi(path: String, readonly: Bool) -> RawDatabase

@external(javascript, "./database_ffi.mjs", "safe_integers")
fn safe_integers(database: RawDatabase) -> Nil

@external(javascript, "./database_ffi.mjs", "execute")
fn execute(database: RawDatabase, sql: String) -> Nil

@external(javascript, "./database_ffi.mjs", "pragma")
fn pragma(database: RawDatabase, sql: String) -> Nil

@external(javascript, "./database_ffi.mjs", "prepare")
fn prepare(database: RawDatabase, sql: String) -> Statement

@external(javascript, "./database_ffi.mjs", "run")
fn run_ffi(statement: Statement, parameters: Parameters) -> #(Int, Int)

@external(javascript, "./database_ffi.mjs", "new_parameters")
fn new_parameters() -> Parameters

@external(javascript, "./database_ffi.mjs", "set_text_parameter")
fn set_text_parameter(
  parameters: Parameters,
  name: String,
  value: String,
) -> Nil

@external(javascript, "./database_ffi.mjs", "set_integer_parameter")
fn set_integer_parameter(
  parameters: Parameters,
  name: String,
  value: Int,
) -> Nil

@external(javascript, "./database_ffi.mjs", "set_blob_parameter")
fn set_blob_parameter(
  parameters: Parameters,
  name: String,
  value: BitArray,
) -> Nil

@external(javascript, "./database_ffi.mjs", "get_count")
fn get_count(statement: Statement) -> Int

@external(javascript, "./database_ffi.mjs", "get_blob_by_text")
fn get_blob_by_text(
  statement: Statement,
  column: String,
  parameter: String,
) -> Option(BitArray)

@external(javascript, "./database_ffi.mjs", "get_blob_by_blob")
fn get_blob_by_blob(
  statement: Statement,
  column: String,
  parameter: BitArray,
) -> Option(BitArray)

@external(javascript, "./database_ffi.mjs", "call_and_catch")
fn call_and_catch(action: fn() -> a) -> Result(a, String)

pub fn open(path: String) -> Database {
  configure(open_ffi(path, False), query_path)
}

pub fn open_readonly(path: String) -> Database {
  prepare_database(open_ffi(path, True), query_path)
}

pub fn open_memory(schema: String) -> Database {
  let database = open_ffi(":memory:", False)
  execute(database, node.read_file(schema))
  configure(database, query_path)
}

pub fn migrate() -> lib_error.Async(Nil, String) {
  fn(done) {
    node.exec_file("dbmate", ["--no-dump-schema", "up"], [
      #("DATABASE_URL", "sqlite:" <> manifest_path),
      #("DBMATE_MIGRATIONS_DIR", migrations_path),
      #("DBMATE_SCHEMA_FILE", schema_path),
    ])(done)
  }
}

pub fn init() -> lib_error.Async(Database, String) {
  fn(done) {
    migrate()(fn(result) {
      case result {
        Ok(Nil) -> done(Ok(open(manifest_path)))
        Error(reason) -> done(Error(reason))
      }
    })
  }
}

fn run(
  statement: Statement,
  text_parameters: List(#(String, String)),
  integer_parameters: List(#(String, Int)),
  blob_parameters: List(#(String, BitArray)),
) -> #(Int, Int) {
  let parameters = new_parameters()
  set_text_parameters(parameters, text_parameters)
  set_integer_parameters(parameters, integer_parameters)
  set_blob_parameters(parameters, blob_parameters)
  run_ffi(statement, parameters)
}

fn set_text_parameters(
  parameters: Parameters,
  values: List(#(String, String)),
) -> Nil {
  case values {
    [] -> Nil
    [#(name, value), ..rest] -> {
      set_text_parameter(parameters, name, value)
      set_text_parameters(parameters, rest)
    }
  }
}

fn set_integer_parameters(
  parameters: Parameters,
  values: List(#(String, Int)),
) -> Nil {
  case values {
    [] -> Nil
    [#(name, value), ..rest] -> {
      set_integer_parameter(parameters, name, value)
      set_integer_parameters(parameters, rest)
    }
  }
}

fn set_blob_parameters(
  parameters: Parameters,
  values: List(#(String, BitArray)),
) -> Nil {
  case values {
    [] -> Nil
    [#(name, value), ..rest] -> {
      set_blob_parameter(parameters, name, value)
      set_blob_parameters(parameters, rest)
    }
  }
}

pub fn upsert_content(
  database: Database,
  content_id: BitArray,
  original: BitArray,
) -> Bool {
  let Database(_, _, _, _, _, _, _, _, statement) = database
  let #(last_insert_rowid, _) =
    run(statement, [], [], [#("id", content_id), #("original", original)])
  last_insert_rowid != 0
}

pub fn update_content_compressed(
  database: Database,
  content_id: BitArray,
  compressed: BitArray,
) -> Nil {
  let Database(_, _, _, _, _, _, _, statement, _) = database
  let _ =
    run(statement, [], [], [#("compressed", compressed), #("id", content_id)])
  Nil
}

pub fn insert_version(database: Database, version_id: Int) -> Nil {
  let Database(_, _, _, _, statement, _, _, _, _) = database
  let _ = run(statement, [], [#("id", version_id)], [])
  Nil
}

pub fn insert_path(
  database: Database,
  path_id: String,
  version_id: Int,
  content_id: BitArray,
) -> Nil {
  let Database(_, _, _, statement, _, _, _, _, _) = database
  let _ =
    run(statement, [#("id", path_id)], [#("version_id", version_id)], [
      #("content_id", content_id),
    ])
  Nil
}

pub fn transaction(database: Database, action: fn() -> a) -> a {
  let Database(
    _,
    begin_transaction,
    commit_transaction,
    _,
    _,
    _,
    rollback_transaction,
    _,
    _,
  ) = database
  let _ = run(begin_transaction, [], [], [])
  case call_and_catch(action) {
    Ok(value) -> {
      let _ = run(commit_transaction, [], [], [])
      value
    }
    Error(reason) -> {
      let _ = run(rollback_transaction, [], [], [])
      panic as reason
    }
  }
}

pub fn query_count(database: Database, path: String) -> Int {
  let Database(raw, ..) = database
  get_count(prepare(raw, node.read_file(path)))
}

pub fn query_blob_by_text(
  database: Database,
  path: String,
  column: String,
  parameter: String,
) -> Result(BitArray, String) {
  let Database(raw, ..) = database
  case get_blob_by_text(prepare(raw, node.read_file(path)), column, parameter) {
    Some(value) -> Ok(value)
    None -> Error("not found")
  }
}

pub fn query_blob_by_blob(
  database: Database,
  path: String,
  column: String,
  parameter: BitArray,
) -> Result(BitArray, String) {
  let Database(raw, ..) = database
  case get_blob_by_blob(prepare(raw, node.read_file(path)), column, parameter) {
    Some(value) -> Ok(value)
    None -> Error("not found")
  }
}

fn configure(database: RawDatabase, query_directory: String) -> Database {
  safe_integers(database)
  pragma(
    database,
    parse_pragma(read_query(query_directory, "pragma_foreign_key.sql")),
  )
  pragma(
    database,
    parse_pragma(read_query(query_directory, "pragma_wal_autocheckpoint.sql")),
  )
  prepare_database(database, query_directory)
}

fn prepare_database(
  database: RawDatabase,
  query_directory: String,
) -> Database {
  Database(
    database,
    prepare(database, read_query(query_directory, "begin_transaction.sql")),
    prepare(database, read_query(query_directory, "commit_transaction.sql")),
    prepare(database, read_query(query_directory, "insert_path.sql")),
    prepare(database, read_query(query_directory, "insert_version.sql")),
    parse_pragma(read_query(query_directory, "pragma_wal_checkpoint.sql")),
    prepare(database, read_query(query_directory, "rollback_transaction.sql")),
    prepare(
      database,
      read_query(query_directory, "update_content_compressed.sql"),
    ),
    prepare(database, read_query(query_directory, "upsert_content.sql")),
  )
}

fn read_query(directory: String, filename: String) -> String {
  node.read_file(directory <> "/" <> filename)
}

fn parse_pragma(value: String) -> String {
  case string.starts_with(value, "pragma ") {
    True -> string.drop_start(value, string.length("pragma "))
    False -> value
  }
}
