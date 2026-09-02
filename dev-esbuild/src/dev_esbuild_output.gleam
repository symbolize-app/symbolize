import dev_esbuild_context as dev_context
import dev_esbuild_db as db
import gleam/bit_array
import gleam/int
import gleam/option.{type Option, None, Some}
import gleam/string
import lib_collection
import lib_hex

const service_worker_main_path = "svc-gateway-guest-run/serviceWorker.js"

const service_worker_shell_path = "svc-gateway-guest-run/serviceWorkerShell.js"

pub type OutputFile {
  OutputFile(original: BitArray, path_id: String)
}

pub type ContentFile {
  ContentFile(content_id: BitArray, original: BitArray, path_id: String)
}

@external(javascript, "./output_ffi.mjs", "sha256")
fn sha256(original: BitArray) -> BitArray

@external(javascript, "./output_ffi.mjs", "brotli_compress")
pub fn compress_content(original: BitArray) -> BitArray

@external(javascript, "./output_ffi.mjs", "brotli_decompress")
pub fn decompress_content(compressed: BitArray) -> BitArray

pub fn output_file(original: BitArray, path_id: String) -> OutputFile {
  OutputFile(original, path_id)
}

pub fn content_file(original: BitArray, path_id: String) -> ContentFile {
  ContentFile(sha256(original), original, path_id)
}

pub fn prepare(
  version_id: Int,
  output_files: List(OutputFile),
) -> Result(List(ContentFile), String) {
  let main_content_files = list_map(output_files, to_content_file)
  let manifest_files =
    lib_collection.group_by(main_content_files, fn(file) {
      manifest_name(file.path_id)
    })
    |> list_map(fn(group) {
      let #(name, content_files) = group
      let manifest_file = manifest(name, content_files)
      content_file(manifest_file.original, manifest_file.path_id)
    })
  case find_path(main_content_files, service_worker_main_path) {
    Error(Nil) -> Error("Service worker main not found")
    Ok(_) -> {
      let shell =
        service_worker_shell(version_id, main_content_files, manifest_files)
      Ok(append(main_content_files, append(manifest_files, [shell])))
    }
  }
}

pub fn path_id(file: ContentFile) -> String {
  file.path_id
}

pub fn original(file: ContentFile) -> BitArray {
  file.original
}

pub fn content_id(file: ContentFile) -> BitArray {
  file.content_id
}

pub fn write(
  context: dev_context.Context,
  version_id: Int,
  output_files: List(OutputFile),
) -> Result(Nil, String) {
  case prepare(version_id, output_files) {
    Error(reason) -> Error(reason)
    Ok(content_files) -> {
      let database = dev_context.database(context)
      persist_content_files(context, database, content_files)
      db.transaction(database, fn() {
        db.insert_version(database, version_id)
        list_each(content_files, fn(file) {
          db.insert_path(database, file.path_id, version_id, file.content_id)
        })
      })
      Ok(Nil)
    }
  }
}

fn persist_content_files(
  context: dev_context.Context,
  database: db.Database,
  content_files: List(ContentFile),
) -> Nil {
  list_each(content_files, fn(file) {
    case db.upsert_content(database, file.content_id, file.original) {
      True ->
        case dev_context.mode(dev_context.dev_context(context)) {
          dev_context.Production ->
            db.update_content_compressed(
              database,
              file.content_id,
              compress_content(file.original),
            )
          dev_context.Development -> Nil
        }
      False -> Nil
    }
  })
}

fn to_content_file(file: OutputFile) -> ContentFile {
  content_file(file.original, file.path_id)
}

fn manifest(name: String, content_files: List(ContentFile)) -> OutputFile {
  let entries = list_map(content_files, manifest_entry)
  let text = "Object.assign(manifest,{" <> join(",", entries) <> "})"
  OutputFile(bit_array.from_string(text), ".manifest/" <> name <> ".js")
}

fn manifest_entry(file: ContentFile) -> String {
  json_string(file.path_id)
  <> ":"
  <> json_string(
    lib_hex.uint8_array_to_hex(file.content_id) <> extension(file.path_id),
  )
}

fn service_worker_shell(
  version_id: Int,
  main_content_files: List(ContentFile),
  manifest_files: List(ContentFile),
) -> ContentFile {
  let all_scripts =
    append(
      list_map(manifest_files, shell_script),
      list_filter_map(main_content_files, fn(file) {
        case file.path_id == service_worker_main_path {
          True -> Some(shell_script(file))
          False -> None
        }
      }),
    )
  let text =
    "version=BigInt("
    <> int.to_string(version_id)
    <> ");manifest={};importScripts("
    <> join(",", all_scripts)
    <> ")"
  content_file(bit_array.from_string(text), service_worker_shell_path)
}

fn shell_script(file: ContentFile) -> String {
  "\"/.code/.id/" <> lib_hex.uint8_array_to_hex(file.content_id) <> ".js\""
}

fn manifest_name(path: String) -> String {
  case string.split_once(path, on: "/") {
    Error(Nil) -> ""
    Ok(#(name, _)) -> name
  }
}

fn find_path(
  files: List(ContentFile),
  expected: String,
) -> Result(ContentFile, Nil) {
  case files {
    [] -> Error(Nil)
    [file, ..rest] ->
      case file.path_id == expected {
        True -> Ok(file)
        False -> find_path(rest, expected)
      }
  }
}

fn extension(path: String) -> String {
  let basename = basename(path)
  case all_dots(basename) {
    True ->
      case string.length(basename) >= 3 {
        True -> "."
        False -> ""
      }
    False ->
      case string.split_once(string.reverse(basename), on: ".") {
        Error(Nil) -> ""
        Ok(#(reversed_extension, before_dot)) ->
          case before_dot == "" {
            True -> ""
            False -> "." <> string.reverse(reversed_extension)
          }
      }
  }
}

fn basename(path: String) -> String {
  case string.split_once(string.reverse(path), on: "/") {
    Error(Nil) -> path
    Ok(#(reversed_basename, _)) -> string.reverse(reversed_basename)
  }
}

fn all_dots(value: String) -> Bool {
  string.replace(value, each: ".", with: "") == ""
}

fn json_string(value: String) -> String {
  "\"" <> json_escape(string.to_utf_codepoints(value), "") <> "\""
}

fn json_escape(values: List(UtfCodepoint), output: String) -> String {
  case values {
    [] -> output
    [first, ..rest] -> json_escape(rest, output <> escape_json_codepoint(first))
  }
}

fn escape_json_codepoint(value: UtfCodepoint) -> String {
  let codepoint = string.utf_codepoint_to_int(value)
  case codepoint {
    8 -> "\\b"
    9 -> "\\t"
    10 -> "\\n"
    12 -> "\\f"
    13 -> "\\r"
    codepoint if codepoint < 32 -> "\\u" <> padded_hex(codepoint)
    34 -> "\\\""
    92 -> "\\\\"
    _ -> string.from_utf_codepoints([value])
  }
}

fn padded_hex(value: Int) -> String {
  let hex = int.to_base16(value)
  case string.length(hex) {
    1 -> "000" <> hex
    2 -> "00" <> hex
    3 -> "0" <> hex
    _ -> hex
  }
}

fn list_map(values: List(a), transform: fn(a) -> b) -> List(b) {
  case values {
    [] -> []
    [first, ..rest] -> [transform(first), ..list_map(rest, transform)]
  }
}

fn list_filter_map(values: List(a), transform: fn(a) -> Option(b)) -> List(b) {
  case values {
    [] -> []
    [first, ..rest] ->
      case transform(first) {
        None -> list_filter_map(rest, transform)
        Some(value) -> [value, ..list_filter_map(rest, transform)]
      }
  }
}

fn append(first: List(a), second: List(a)) -> List(a) {
  case first {
    [] -> second
    [head, ..tail] -> [head, ..append(tail, second)]
  }
}

fn join(separator: String, values: List(String)) -> String {
  case values {
    [] -> ""
    [first, ..rest] -> join_nonempty(separator, rest, first)
  }
}

fn join_nonempty(
  separator: String,
  values: List(String),
  output: String,
) -> String {
  case values {
    [] -> output
    [first, ..rest] ->
      join_nonempty(separator, rest, output <> separator <> first)
  }
}

fn list_each(values: List(a), effect: fn(a) -> Nil) -> Nil {
  case values {
    [] -> Nil
    [first, ..rest] -> {
      effect(first)
      list_each(rest, effect)
    }
  }
}
