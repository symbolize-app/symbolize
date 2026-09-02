import dev_esbuild_context as dev_context
import dev_esbuild_db as db
import dev_esbuild_modules as modules
import dev_esbuild_output as output
import gleam/bit_array
import gleam/io
import gleam/result
import gleam/string
import lib_hex
import lib_time

const schema_path = "../svc-gateway-host-store/schema.sql"

const content_count_query = "test/query/count_content.sql"

const version_count_query = "test/query/count_version.sql"

const path_count_query = "test/query/count_path.sql"

const compressed_count_query = "test/query/count_compressed.sql"

pub fn main() {
  let source = bit_array.from_string("content to compress")
  assert output.compress_content(source)
    |> output.decompress_content
    |> bit_array.to_string
    == Ok("content to compress")

  let hello = output.content_file(bit_array.from_string("hello"), "x/a.js")
  assert lib_hex.uint8_array_to_hex(output.content_id(hello))
    == "2cf24dba5fb0a30e26e83b2ac5b9e29e1b161e5c1fa7425e73043362938b9824"

  let files = [
    output.output_file(
      bit_array.from_string("worker"),
      "svc-gateway-guest-run/serviceWorker.js",
    ),
    output.output_file(
      bit_array.from_string("main"),
      "svc-gateway-guest-run/main.mjs",
    ),
    output.output_file(bit_array.from_string("a"), "lib-one/a.js"),
    output.output_file(bit_array.from_string("b"), "lib-two/b.css"),
  ]
  let assert Ok(content_files) = output.prepare(123, files)
  assert list_paths(content_files)
    == [
      "svc-gateway-guest-run/serviceWorker.js",
      "svc-gateway-guest-run/main.mjs",
      "lib-one/a.js",
      "lib-two/b.css",
      ".manifest/svc-gateway-guest-run.js",
      ".manifest/lib-one.js",
      ".manifest/lib-two.js",
      "svc-gateway-guest-run/serviceWorkerShell.js",
    ]
  let lib_manifest = find(content_files, ".manifest/lib-one.js")
  assert bit_array.to_string(output.original(lib_manifest))
    |> result.unwrap(or: "")
    == "Object.assign(manifest,{\"lib-one/a.js\":\"ca978112ca1bbdcafac231b39a23dc4da786eff8147c4e72b9807785afee48bb.js\"})"

  let special_path = "lib-\u{8}\u{c}\u{0}\u{1}/a.js"
  let special_source = bit_array.from_string("special")
  let special_id =
    output.content_file(special_source, special_path)
    |> output.content_id
    |> lib_hex.uint8_array_to_hex
  let special_files = [
    output.output_file(
      bit_array.from_string("worker"),
      "svc-gateway-guest-run/serviceWorker.js",
    ),
    output.output_file(special_source, special_path),
  ]
  let assert Ok(special_content_files) = output.prepare(123, special_files)
  let special_manifest_path = ".manifest/lib-\u{8}\u{c}\u{0}\u{1}.js"
  let special_manifest = find(special_content_files, special_manifest_path)
  assert bit_array.to_string(output.original(special_manifest))
    |> result.unwrap(or: "")
    == "Object.assign(manifest,{\"lib-\\b\\f\\u0000\\u0001/a.js\":\""
    <> special_id
    <> ".js\"})"

  let extension_source = bit_array.from_string("extension")
  let extension_id =
    output.content_file(extension_source, "lib.with/.entry")
    |> output.content_id
    |> lib_hex.uint8_array_to_hex
  let extension_files = [
    output.output_file(
      bit_array.from_string("worker"),
      "svc-gateway-guest-run/serviceWorker.js",
    ),
    output.output_file(extension_source, "lib.with/.entry"),
  ]
  let assert Ok(extension_content_files) = output.prepare(123, extension_files)
  let extension_manifest =
    find(extension_content_files, ".manifest/lib.with.js")
  assert bit_array.to_string(output.original(extension_manifest))
    |> result.unwrap(or: "")
    == "Object.assign(manifest,{\"lib.with/.entry\":\""
    <> extension_id
    <> "\"})"

  let shell = last(content_files)
  assert bit_array.to_string(output.original(shell))
    |> result.unwrap(or: "")
    |> string.contains("version=BigInt(123);manifest={};importScripts(")

  let database = db.open_memory(schema_path)
  let context =
    dev_context.context(
      dev_context.dev(dev_context.Production, "/workspace/build"),
      database,
      lib_time.new_context(
        lib_time.custom(fn() { 0.0 }, fn(_callback, _ms) { Nil }),
      ),
    )
  assert output.write(context, 123, files) == Ok(Nil)
  assert db.query_count(database, content_count_query) == 8
  assert db.query_count(database, version_count_query) == 1
  assert db.query_count(database, path_count_query) == 8
  assert db.query_count(database, compressed_count_query) == 8

  let development_database = db.open_memory(schema_path)
  let development_context =
    dev_context.context(
      dev_context.dev(dev_context.Development, "/workspace/build"),
      development_database,
      lib_time.new_context(
        lib_time.custom(fn() { 0.0 }, fn(_callback, _ms) { Nil }),
      ),
    )
  assert output.write(development_context, 124, files) == Ok(Nil)
  assert db.query_count(development_database, compressed_count_query) == 0

  let module_context = modules.context("/workspace")
  let assert Ok(#(module_context, module_path)) =
    modules.convert_to_out_path(
      module_context,
      "/workspace/node_modules/.pnpm/pkg@1.0.0/node_modules/pkg/src/main.js",
    )
  assert module_path == ".pnpm-pkg/src/main.js"
  let assert Ok(#(module_context, _)) =
    modules.convert_to_out_path(
      module_context,
      "/workspace/node_modules/.pnpm/pkg@1.0.0/node_modules/pkg/src/other.js",
    )
  let assert Error(message) =
    modules.convert_to_out_path(
      module_context,
      "/workspace/node_modules/.pnpm/pkg@2.0.0/node_modules/pkg/src/other.js",
    )
  assert message == "Ambiguous versions found for pkg: 1.0.0 / 2.0.0"
  assert modules.relative_import("svc/main.mjs", ".pnpm-pkg/src/main.mjs")
    == "../.pnpm-pkg/src/main.mjs"
  assert modules.relative_import("entry.mjs", "dependency.mjs")
    == "./dependency.mjs"
  assert modules.relative_import(
      "lib-dataflow/index",
      "lib-dataflow/.gleam/ffi",
    )
    == "./.gleam/ffi"
  let assert Ok(#(module_context, development_path)) =
    modules.convert_to_out_path(
      module_context,
      "/workspace/svc-gateway-guest-run/build/dev/javascript/"
        <> "symbolize_svc_gateway_guest_run/"
        <> "svc_gateway_guest_run_development.mjs",
    )
  assert development_path == "svc-gateway-guest-run/main.development"
  let assert Ok(#(_module_context, payload_path)) =
    modules.convert_to_out_path(
      module_context,
      "/workspace/svc-gateway-guest-run/build/dev/javascript/"
        <> "symbolize_lib_payload/lib_payload.mjs",
    )
  assert payload_path == "lib-payload/index"

  let resolver =
    modules.resolver(modules.context("/workspace"), ["/workspace/entry.js"])
  let assert Ok(#(resolver, rewritten)) =
    modules.rewrite_resolved_path(
      resolver,
      "/workspace/entry.js",
      "/workspace/node_modules/.pnpm/pkg@1.0.0/node_modules/pkg/src/main.js",
    )
  assert rewritten == ".pnpm-pkg/src/main.js.mjs"
  assert modules.resolver_new_entry_points(resolver)
    == [
      "/workspace/node_modules/.pnpm/pkg@1.0.0/node_modules/pkg/src/main.js",
    ]
  let assert Ok(#(resolver, _)) =
    modules.rewrite_resolved_path(
      resolver,
      "/workspace/entry.js",
      "/workspace/node_modules/.pnpm/pkg@1.0.0/node_modules/pkg/src/main.js",
    )
  assert modules.resolver_new_entry_points(resolver)
    == [
      "/workspace/node_modules/.pnpm/pkg@1.0.0/node_modules/pkg/src/main.js",
    ]

  io.println("dev-esbuild output Gleam parity tests passed")
}

fn list_paths(files: List(output.ContentFile)) -> List(String) {
  case files {
    [] -> []
    [first, ..rest] -> [output.path_id(first), ..list_paths(rest)]
  }
}

fn last(files: List(a)) -> a {
  case files {
    [only] -> only
    [_, ..rest] -> last(rest)
    [] -> panic as "expected non-empty list"
  }
}

fn find(
  files: List(output.ContentFile),
  expected_path: String,
) -> output.ContentFile {
  case files {
    [first, ..rest] ->
      case output.path_id(first) == expected_path {
        True -> first
        False -> find(rest, expected_path)
      }
    [] -> panic as "expected content file"
  }
}
