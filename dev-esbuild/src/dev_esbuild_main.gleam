import dev_esbuild_build as build
import dev_esbuild_context as dev_context
import dev_esbuild_db as db
import dev_esbuild_modules as modules
import dev_esbuild_node as node
import dev_esbuild_output as output
import gleam/float
import gleam/int
import gleam/io
import gleam/option.{type Option, None, Some}
import gleam/string
import lib_time

pub type Arguments {
  Arguments(
    mode: dev_context.Mode,
    database: String,
    schema: String,
    migrations: String,
    query: String,
    outbase: String,
    guest_dir: String,
    esbuild_bin: String,
    better_sqlite3_binding: String,
    copy_entries: List(String),
    classic_entries: List(String),
    module_entries: List(String),
    packages: List(#(String, String)),
  )
}

pub fn main() {
  case parse_arguments(node.argv()) {
    Error(reason) -> fail(reason)
    Ok(arguments) -> run(arguments)
  }
}

fn run(arguments: Arguments) -> Nil {
  let Arguments(
    mode,
    database,
    schema,
    migrations,
    query,
    outbase,
    guest_dir,
    esbuild_bin,
    better_sqlite3_binding,
    copy_entries,
    classic_entries,
    module_entries,
    packages,
  ) = arguments

  let manifest_path = node.resolve(database)
  let schema_path = node.resolve(schema)
  let migrations_path = node.resolve(migrations)
  let query_path = node.resolve(query)
  let resolved_outbase = node.resolve(outbase)
  let outdir = node.resolve(database <> ".guest")

  build.set_guest_dir(node.resolve(guest_dir))
  build.set_esbuild_bin(node.resolve(esbuild_bin))
  build.set_better_sqlite3_binding(node.resolve(better_sqlite3_binding))

  list_each(packages, fn(pair) {
    let #(name, path) = pair
    build.set_package_path(name, node.resolve(path))
  })

  db.init_with_paths(manifest_path, schema_path, migrations_path, query_path)(
    fn(result) {
      case result {
        Error(reason) -> fail(reason)
        Ok(database_instance) -> {
          let time = lib_time.new_context(lib_time.time())
          let context =
            dev_context.context(
              dev_context.dev(mode, outdir),
              database_instance,
              time,
            )
          build_files_configured(
            context,
            resolved_outbase,
            copy_entries,
            classic_entries,
            module_entries,
          )
        }
      }
    },
  )
}

fn build_files_configured(
  context: dev_context.Context,
  outbase: String,
  copy_entries: List(String),
  classic_entries: List(String),
  module_entries: List(String),
) -> Nil {
  let mode = dev_context.mode(dev_context.dev_context(context))
  let production = case mode {
    dev_context.Development -> False
    dev_context.Production -> True
  }

  let copy_entry_points = list_map(copy_entries, node.resolve)
  let classic_entry_points = list_map(classic_entries, node.resolve)
  let module_entry_points = list_map(module_entries, node.resolve)

  let start = lib_time.performance_now(dev_context.time(context))
  let version_id = node.now_milliseconds()
  build_files(
    copy_entry_points,
    classic_entry_points,
    module_entry_points,
    outbase,
    dev_context.outdir(dev_context.dev_context(context)),
    production,
    fn(result) { finish_build(context, start, version_id, result) },
  )
}

fn build_files(
  copy_entry_points: List(String),
  classic_entry_points: List(String),
  module_entry_points: List(String),
  outbase: String,
  outdir: String,
  production: Bool,
  done: fn(Result(build.BuildResult, String)) -> Nil,
) -> Nil {
  let resolver = modules.resolver(modules.context(outbase), module_entry_points)
  build.build_all(
    copy_entry_points,
    classic_entry_points,
    module_entry_points,
    outbase,
    outdir,
    production,
    resolver,
  )(fn(result) {
    case result {
      Error(reason) -> done(Error(reason))
      Ok(#(copy_result, classic_result, module_result)) ->
        done(Ok(merge_results(copy_result, classic_result, module_result)))
    }
  })
}

fn finish_build(
  context: dev_context.Context,
  start: Float,
  version_id: Int,
  result: Result(build.BuildResult, String),
) -> Nil {
  let end = lib_time.performance_now(dev_context.time(context))
  case result {
    Error(reason) -> fail(reason)
    Ok(#(errors, files, _warnings)) ->
      case errors {
        [] -> {
          let output_files =
            list_map(files, fn(file) {
              let #(contents, path) = file
              output.output_file(
                contents,
                node.relative(
                  dev_context.outdir(dev_context.dev_context(context)),
                  path,
                ),
              )
            })
          case output.write(context, version_id, output_files) {
            Error(reason) -> fail(reason)
            Ok(Nil) -> Nil
          }
        }
        _ -> {
          print_messages(errors)
          node.set_failure()
        }
      }
  }
  io.println(
    "Done build "
    <> int.to_string(version_id)
    <> ": "
    <> int.to_string(float.round(end -. start))
    <> "ms",
  )
}

fn parse_arguments(arguments: List(String)) -> Result(Arguments, String) {
  parse_arguments_loop(
    arguments,
    dev_context.Development,
    None,
    None,
    None,
    None,
    None,
    None,
    None,
    None,
    [],
    [],
    [],
    [],
  )
}

fn parse_arguments_loop(
  arguments: List(String),
  mode: dev_context.Mode,
  database: Option(String),
  schema: Option(String),
  migrations: Option(String),
  query: Option(String),
  outbase: Option(String),
  guest_dir: Option(String),
  esbuild_bin: Option(String),
  better_sqlite3_binding: Option(String),
  copy_entries: List(String),
  classic_entries: List(String),
  module_entries: List(String),
  packages: List(#(String, String)),
) -> Result(Arguments, String) {
  case arguments {
    [] ->
      case
        mode,
        database,
        schema,
        migrations,
        query,
        guest_dir,
        esbuild_bin,
        better_sqlite3_binding
      {
        _, None, _, _, _, _, _, _ ->
          Error("Missing required option '--database <path>'")
        _, _, None, _, _, _, _, _ ->
          Error("Missing required option '--schema <path>'")
        _, _, _, None, _, _, _, _ ->
          Error("Missing required option '--migrations <path>'")
        _, _, _, _, None, _, _, _ ->
          Error("Missing required option '--query <path>'")
        _, _, _, _, _, None, _, _ ->
          Error("Missing required option '--guest-dir <path>'")
        _, _, _, _, _, _, None, _ ->
          Error("Missing required option '--esbuild-bin <path>'")
        _, _, _, _, _, _, _, None ->
          Error("Missing required option '--better-sqlite3-binding <path>'")
        parsed_mode,
          Some(db_path),
          Some(schema_file),
          Some(migrations_dir),
          Some(query_dir),
          Some(guest_directory),
          Some(bin),
          Some(binding)
        -> {
          let outbase_path = case outbase {
            Some(p) -> p
            None -> "."
          }
          Ok(Arguments(
            parsed_mode,
            db_path,
            schema_file,
            migrations_dir,
            query_dir,
            outbase_path,
            guest_directory,
            bin,
            binding,
            copy_entries,
            classic_entries,
            module_entries,
            packages,
          ))
        }
      }
    ["--mode", value, ..rest] ->
      case parse_mode(value) {
        Error(reason) -> Error(reason)
        Ok(parsed_mode) ->
          parse_arguments_loop(
            rest,
            parsed_mode,
            database,
            schema,
            migrations,
            query,
            outbase,
            guest_dir,
            esbuild_bin,
            better_sqlite3_binding,
            copy_entries,
            classic_entries,
            module_entries,
            packages,
          )
      }
    ["--mode"] -> Error("Option '--mode <value>' argument missing")
    ["--database", value, ..rest] ->
      parse_arguments_loop(
        rest,
        mode,
        Some(value),
        schema,
        migrations,
        query,
        outbase,
        guest_dir,
        esbuild_bin,
        better_sqlite3_binding,
        copy_entries,
        classic_entries,
        module_entries,
        packages,
      )
    ["--database"] -> Error("Option '--database <value>' argument missing")
    ["--schema", value, ..rest] ->
      parse_arguments_loop(
        rest,
        mode,
        database,
        Some(value),
        migrations,
        query,
        outbase,
        guest_dir,
        esbuild_bin,
        better_sqlite3_binding,
        copy_entries,
        classic_entries,
        module_entries,
        packages,
      )
    ["--schema"] -> Error("Option '--schema <value>' argument missing")
    ["--migrations", value, ..rest] ->
      parse_arguments_loop(
        rest,
        mode,
        database,
        schema,
        Some(value),
        query,
        outbase,
        guest_dir,
        esbuild_bin,
        better_sqlite3_binding,
        copy_entries,
        classic_entries,
        module_entries,
        packages,
      )
    ["--migrations"] -> Error("Option '--migrations <value>' argument missing")
    ["--query", value, ..rest] ->
      parse_arguments_loop(
        rest,
        mode,
        database,
        schema,
        migrations,
        Some(value),
        outbase,
        guest_dir,
        esbuild_bin,
        better_sqlite3_binding,
        copy_entries,
        classic_entries,
        module_entries,
        packages,
      )
    ["--query"] -> Error("Option '--query <value>' argument missing")
    ["--outbase", value, ..rest] ->
      parse_arguments_loop(
        rest,
        mode,
        database,
        schema,
        migrations,
        query,
        Some(value),
        guest_dir,
        esbuild_bin,
        better_sqlite3_binding,
        copy_entries,
        classic_entries,
        module_entries,
        packages,
      )
    ["--outbase"] -> Error("Option '--outbase <value>' argument missing")
    ["--guest-dir", value, ..rest] ->
      parse_arguments_loop(
        rest,
        mode,
        database,
        schema,
        migrations,
        query,
        outbase,
        Some(value),
        esbuild_bin,
        better_sqlite3_binding,
        copy_entries,
        classic_entries,
        module_entries,
        packages,
      )
    ["--guest-dir"] -> Error("Option '--guest-dir <value>' argument missing")
    ["--esbuild-bin", value, ..rest] ->
      parse_arguments_loop(
        rest,
        mode,
        database,
        schema,
        migrations,
        query,
        outbase,
        guest_dir,
        Some(value),
        better_sqlite3_binding,
        copy_entries,
        classic_entries,
        module_entries,
        packages,
      )
    ["--esbuild-bin"] ->
      Error("Option '--esbuild-bin <value>' argument missing")
    ["--better-sqlite3-binding", value, ..rest] ->
      parse_arguments_loop(
        rest,
        mode,
        database,
        schema,
        migrations,
        query,
        outbase,
        guest_dir,
        esbuild_bin,
        Some(value),
        copy_entries,
        classic_entries,
        module_entries,
        packages,
      )
    ["--better-sqlite3-binding"] ->
      Error("Option '--better-sqlite3-binding <value>' argument missing")
    ["--copy-entry", value, ..rest] ->
      parse_arguments_loop(
        rest,
        mode,
        database,
        schema,
        migrations,
        query,
        outbase,
        guest_dir,
        esbuild_bin,
        better_sqlite3_binding,
        append(copy_entries, [value]),
        classic_entries,
        module_entries,
        packages,
      )
    ["--copy-entry"] -> Error("Option '--copy-entry <value>' argument missing")
    ["--classic-entry", value, ..rest] ->
      parse_arguments_loop(
        rest,
        mode,
        database,
        schema,
        migrations,
        query,
        outbase,
        guest_dir,
        esbuild_bin,
        better_sqlite3_binding,
        copy_entries,
        append(classic_entries, [value]),
        module_entries,
        packages,
      )
    ["--classic-entry"] ->
      Error("Option '--classic-entry <value>' argument missing")
    ["--module-entry", value, ..rest] ->
      parse_arguments_loop(
        rest,
        mode,
        database,
        schema,
        migrations,
        query,
        outbase,
        guest_dir,
        esbuild_bin,
        better_sqlite3_binding,
        copy_entries,
        classic_entries,
        append(module_entries, [value]),
        packages,
      )
    ["--module-entry"] ->
      Error("Option '--module-entry <value>' argument missing")
    ["--package", value, ..rest] ->
      case string.split_once(value, on: "=") {
        Error(Nil) ->
          Error("Option '--package <name=path>' must contain '=' separator")
        Ok(#(name, path)) ->
          parse_arguments_loop(
            rest,
            mode,
            database,
            schema,
            migrations,
            query,
            outbase,
            guest_dir,
            esbuild_bin,
            better_sqlite3_binding,
            copy_entries,
            classic_entries,
            module_entries,
            append(packages, [#(name, path)]),
          )
      }
    ["--package"] -> Error("Option '--package <name=path>' argument missing")
    ["--", ..rest] ->
      case rest {
        [] ->
          parse_arguments_loop(
            [],
            mode,
            database,
            schema,
            migrations,
            query,
            outbase,
            guest_dir,
            esbuild_bin,
            better_sqlite3_binding,
            copy_entries,
            classic_entries,
            module_entries,
            packages,
          )
        [first, ..] ->
          Error(
            "Unexpected argument '"
            <> first
            <> "'. This command does not take positional arguments",
          )
      }
    [argument, ..] ->
      case string.starts_with(argument, "-") {
        True -> Error("Unknown option '" <> argument <> "'")
        False ->
          Error(
            "Unexpected argument '"
            <> argument
            <> "'. This command does not take positional arguments",
          )
      }
  }
}

fn parse_mode(value: String) -> Result(dev_context.Mode, String) {
  case value {
    "development" -> Ok(dev_context.Development)
    "production" -> Ok(dev_context.Production)
    _ ->
      Error(
        "Invalid string option (not \"development\" | \"production\") at (root)",
      )
  }
}

fn merge_results(
  first: build.BuildResult,
  second: build.BuildResult,
  third: build.BuildResult,
) -> build.BuildResult {
  let #(first_errors, first_files, first_warnings) = first
  let #(second_errors, second_files, second_warnings) = second
  let #(third_errors, third_files, third_warnings) = third
  #(
    append(first_errors, append(second_errors, third_errors)),
    append(first_files, append(second_files, third_files)),
    append(first_warnings, append(second_warnings, third_warnings)),
  )
}

fn print_messages(messages: List(String)) -> Nil {
  case messages {
    [] -> Nil
    [first, ..rest] -> {
      io.println(first)
      print_messages(rest)
    }
  }
}

fn fail(reason: String) -> Nil {
  io.println(reason)
  node.set_failure()
}

fn list_map(values: List(a), transform: fn(a) -> b) -> List(b) {
  case values {
    [] -> []
    [first, ..rest] -> [transform(first), ..list_map(rest, transform)]
  }
}

fn append(first: List(a), second: List(a)) -> List(a) {
  case first {
    [] -> second
    [head, ..tail] -> [head, ..append(tail, second)]
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
