import dev_esbuild_modules as modules
import gleam/option.{type Option, None, Some}
import gleam/string
import lib_dataflow as dataflow
import lib_error

pub type BuildResult =
  #(List(String), List(#(BitArray, String)), List(String))

type EntryPoint

type NativeBuildResult

type NativeMessage

type NativeOutputFile

type NativeResolveArgs

type NativeResolveResult

type BuildOptions

type Plugin

@external(javascript, "./build_ffi.mjs", "new_options")
fn new_options() -> BuildOptions

@external(javascript, "./build_ffi.mjs", "set_bundle")
fn set_bundle(options: BuildOptions, value: Bool) -> Nil

@external(javascript, "./build_ffi.mjs", "set_entry_points")
fn set_entry_points(options: BuildOptions, value: List(a)) -> Nil

@external(javascript, "./build_ffi.mjs", "set_outbase")
fn set_outbase(options: BuildOptions, value: String) -> Nil

@external(javascript, "./build_ffi.mjs", "set_outdir")
fn set_outdir(options: BuildOptions, value: String) -> Nil

@external(javascript, "./build_ffi.mjs", "set_define")
fn set_define(options: BuildOptions, key: String, value: String) -> Nil

@external(javascript, "./build_ffi.mjs", "set_external")
fn set_external(options: BuildOptions, value: List(String)) -> Nil

@external(javascript, "./build_ffi.mjs", "set_log_level")
fn set_log_level(options: BuildOptions, value: String) -> Nil

@external(javascript, "./build_ffi.mjs", "set_minify")
fn set_minify(options: BuildOptions, value: Bool) -> Nil

@external(javascript, "./build_ffi.mjs", "set_platform")
fn set_platform(options: BuildOptions, value: String) -> Nil

@external(javascript, "./build_ffi.mjs", "set_write")
fn set_write(options: BuildOptions, value: Bool) -> Nil

@external(javascript, "./build_ffi.mjs", "set_loader")
fn set_loader(options: BuildOptions, extension: String, value: String) -> Nil

@external(javascript, "./build_ffi.mjs", "set_format")
fn set_format(options: BuildOptions, value: String) -> Nil

@external(javascript, "./build_ffi.mjs", "set_entry_names")
fn set_entry_names(options: BuildOptions, value: String) -> Nil

@external(javascript, "./build_ffi.mjs", "set_footer")
fn set_footer(options: BuildOptions, value: String) -> Nil

@external(javascript, "./build_ffi.mjs", "set_global_name")
fn set_global_name(options: BuildOptions, value: String) -> Nil

@external(javascript, "./build_ffi.mjs", "set_out_extension")
fn set_out_extension(
  options: BuildOptions,
  extension: String,
  value: String,
) -> Nil

@external(javascript, "./build_ffi.mjs", "set_plugins")
fn set_plugins(options: BuildOptions, value: List(Plugin)) -> Nil

@external(javascript, "./build_ffi.mjs", "new_entry_point")
fn new_entry_point(input: String, output: String) -> EntryPoint

@external(javascript, "./build_ffi.mjs", "new_modules_plugin")
fn new_modules_plugin(
  rewrite: fn(NativeResolveArgs, NativeResolveResult) ->
    Result(Option(String), String),
) -> Plugin

@external(javascript, "./build_ffi.mjs", "build")
fn build_ffi(
  options: BuildOptions,
  done: fn(Result(NativeBuildResult, String)) -> Nil,
) -> Nil

@external(javascript, "./build_ffi.mjs", "result_errors")
fn result_errors(result: NativeBuildResult) -> List(NativeMessage)

@external(javascript, "./build_ffi.mjs", "result_output_files")
fn result_output_files(result: NativeBuildResult) -> List(NativeOutputFile)

@external(javascript, "./build_ffi.mjs", "result_warnings")
fn result_warnings(result: NativeBuildResult) -> List(NativeMessage)

@external(javascript, "./build_ffi.mjs", "message_text")
fn message_text(message: NativeMessage) -> String

@external(javascript, "./build_ffi.mjs", "output_file_contents")
fn output_file_contents(file: NativeOutputFile) -> BitArray

@external(javascript, "./build_ffi.mjs", "output_file_path")
fn output_file_path(file: NativeOutputFile) -> String

@external(javascript, "./build_ffi.mjs", "resolve_args_importer")
fn resolve_args_importer(args: NativeResolveArgs) -> String

@external(javascript, "./build_ffi.mjs", "resolve_args_kind")
fn resolve_args_kind(args: NativeResolveArgs) -> String

@external(javascript, "./build_ffi.mjs", "resolve_result_external")
fn resolve_result_external(result: NativeResolveResult) -> Bool

@external(javascript, "./build_ffi.mjs", "resolve_result_has_errors")
fn resolve_result_has_errors(result: NativeResolveResult) -> Bool

@external(javascript, "./build_ffi.mjs", "resolve_result_path")
fn resolve_result_path(result: NativeResolveResult) -> String

pub fn build_copy(
  entry_points: List(String),
  outbase: String,
  outdir: String,
  production: Bool,
) -> lib_error.Async(BuildResult, String) {
  fn(done) {
    let options = copy_options(entry_points, outbase, outdir, production)
    build_ffi(options, fn(result) { done(map_build_result(result)) })
  }
}

pub fn build_classic(
  entry_points: List(String),
  outbase: String,
  outdir: String,
  production: Bool,
) -> lib_error.Async(BuildResult, String) {
  fn(done) {
    case has_service_worker(entry_points) {
      True ->
        build_ffi(
          service_worker_classic_options(
            entry_points,
            outbase,
            outdir,
            production,
          ),
          fn(result) { done(map_build_result(result)) },
        )
      False ->
        build_ffi(
          classic_options(entry_points, outbase, outdir, production),
          fn(result) { done(map_build_result(result)) },
        )
    }
  }
}

pub fn build_modules(
  entry_points: List(String),
  outbase: String,
  outdir: String,
  production: Bool,
  resolver: modules.Resolver,
) -> lib_error.Async(BuildResult, String) {
  fn(done) {
    build_modules_loop(
      entry_points,
      resolver,
      outbase,
      outdir,
      production,
      #([], [], []),
      done,
    )
  }
}

fn build_modules_loop(
  input_entry_points: List(String),
  resolver: modules.Resolver,
  outbase: String,
  outdir: String,
  production: Bool,
  accumulated: BuildResult,
  done: fn(Result(BuildResult, String)) -> Nil,
) -> Nil {
  case modules.output_entry_points(resolver, input_entry_points) {
    Error(reason) -> done(Error(reason))
    Ok(#(resolver, entry_points)) -> {
      let context = dataflow.dataflow()
      let state = dataflow.state(resolver)
      let options =
        module_options(
          new_entry_points(entry_points),
          outbase,
          outdir,
          production,
        )
      let plugin =
        new_modules_plugin(fn(args, resolve_result) {
          case should_rewrite(args, resolve_result) {
            False -> Ok(None)
            True -> {
              let resolver = read_resolver(state)
              case
                modules.rewrite_resolved_path(
                  resolver,
                  resolve_args_importer(args),
                  resolve_result_path(resolve_result),
                )
              {
                Error(reason) -> Error(reason)
                Ok(#(resolver, output_path)) -> {
                  write_resolver(context, state, resolver)
                  Ok(Some(output_path))
                }
              }
            }
          }
        })
      set_plugins(options, [plugin])
      build_ffi(options, fn(result) {
        case result {
          Error(reason) -> done(Error(reason))
          Ok(native_result) -> {
            let build_result = normalize_result(native_result)
            let resolver = read_resolver(state)
            let accumulated = merge_results(accumulated, build_result)
            let #(resolver, new_entry_points) =
              modules.take_new_entry_points(resolver)
            case new_entry_points {
              [] -> done(Ok(accumulated))
              _ ->
                build_modules_loop(
                  new_entry_points,
                  resolver,
                  outbase,
                  outdir,
                  production,
                  accumulated,
                  done,
                )
            }
          }
        }
      })
    }
  }
}

fn common_options(
  entry_points: List(a),
  outbase: String,
  outdir: String,
  production: Bool,
) -> BuildOptions {
  let options = new_options()
  set_bundle(options, True)
  set_entry_points(options, entry_points)
  set_outbase(options, outbase)
  set_outdir(options, outdir)
  set_define(options, "import.meta.env.NODE_ENV", environment(production))
  set_external(options, ["timers", "util"])
  set_log_level(options, "warning")
  set_minify(options, production)
  set_platform(options, "browser")
  set_write(options, False)
  set_loader(options, ".css", "text")
  set_loader(options, ".html", "text")
  set_loader(options, ".txt", "text")
  options
}

fn environment(production: Bool) -> String {
  case production {
    True -> "\"production\""
    False -> "\"development\""
  }
}

fn copy_options(
  entry_points: List(String),
  outbase: String,
  outdir: String,
  production: Bool,
) -> BuildOptions {
  let options = common_options(entry_points, outbase, outdir, production)
  set_loader(options, ".html", "copy")
  set_loader(options, ".woff2", "copy")
  options
}

fn classic_options(
  entry_points: List(String),
  outbase: String,
  outdir: String,
  production: Bool,
) -> BuildOptions {
  let options = common_options(entry_points, outbase, outdir, production)
  set_format(options, "iife")
  options
}

fn service_worker_classic_options(
  entry_points: List(String),
  outbase: String,
  outdir: String,
  production: Bool,
) -> BuildOptions {
  let options = classic_options(entry_points, outbase, outdir, production)
  set_entry_names(options, "svc-gateway-guest-run/serviceWorker")
  set_footer(options, "__symbolize_service_worker__.main();")
  set_global_name(options, "__symbolize_service_worker__")
  options
}

fn module_options(
  entry_points: List(EntryPoint),
  outbase: String,
  outdir: String,
  production: Bool,
) -> BuildOptions {
  let options = common_options(entry_points, outbase, outdir, production)
  set_format(options, "esm")
  set_out_extension(options, ".js", ".mjs")
  options
}

fn read_resolver(
  state: dataflow.Mutation(modules.Resolver),
) -> modules.Resolver {
  dataflow.value(dataflow.to_computation(dataflow.mutation(state)))
}

fn write_resolver(
  context: dataflow.Context,
  state: dataflow.Mutation(modules.Resolver),
  resolver: modules.Resolver,
) -> Nil {
  let assert Ok(Nil) =
    dataflow.txn(context, fn() { dataflow.set(context, state, resolver) })
  Nil
}

fn merge_results(first: BuildResult, second: BuildResult) -> BuildResult {
  let #(first_errors, first_files, first_warnings) = first
  let #(second_errors, second_files, second_warnings) = second
  #(
    append(first_errors, second_errors),
    append(first_files, second_files),
    append(first_warnings, second_warnings),
  )
}

fn append(first: List(a), second: List(a)) -> List(a) {
  case first {
    [] -> second
    [head, ..tail] -> [head, ..append(tail, second)]
  }
}

fn new_entry_points(values: List(#(String, String))) -> List(EntryPoint) {
  case values {
    [] -> []
    [#(input, output), ..rest] -> [
      new_entry_point(input, output),
      ..new_entry_points(rest)
    ]
  }
}

fn map_build_result(
  result: Result(NativeBuildResult, String),
) -> Result(BuildResult, String) {
  case result {
    Error(reason) -> Error(reason)
    Ok(value) -> Ok(normalize_result(value))
  }
}

fn normalize_result(result: NativeBuildResult) -> BuildResult {
  #(
    map_messages(result_errors(result)),
    map_output_files(result_output_files(result)),
    map_messages(result_warnings(result)),
  )
}

fn map_messages(messages: List(NativeMessage)) -> List(String) {
  case messages {
    [] -> []
    [first, ..rest] -> [message_text(first), ..map_messages(rest)]
  }
}

fn map_output_files(
  files: List(NativeOutputFile),
) -> List(#(BitArray, String)) {
  case files {
    [] -> []
    [first, ..rest] -> [
      #(output_file_contents(first), output_file_path(first)),
      ..map_output_files(rest)
    ]
  }
}

fn should_rewrite(
  args: NativeResolveArgs,
  resolve_result: NativeResolveResult,
) -> Bool {
  !resolve_result_has_errors(resolve_result)
  && !resolve_result_external(resolve_result)
  && is_import_kind(resolve_args_kind(args))
}

fn is_import_kind(kind: String) -> Bool {
  kind == "import-statement" || kind == "dynamic-import"
}

pub fn build_all(
  copy_entry_points: List(String),
  classic_entry_points: List(String),
  module_entry_points: List(String),
  outbase: String,
  outdir: String,
  production: Bool,
  resolver: modules.Resolver,
) -> lib_error.Async(#(BuildResult, BuildResult, BuildResult), String) {
  fn(done) {
    let context = dataflow.dataflow()
    let state = dataflow.state(JoinState(None, None, None, False))
    build_copy(copy_entry_points, outbase, outdir, production)(fn(result) {
      record(context, state, First(result), done)
    })
    build_classic(classic_entry_points, outbase, outdir, production)(fn(result) {
      record(context, state, Second(result), done)
    })
    build_modules(module_entry_points, outbase, outdir, production, resolver)(
      fn(result) { record(context, state, Third(result), done) },
    )
  }
}

type JoinState {
  JoinState(
    first: Option(Result(BuildResult, String)),
    second: Option(Result(BuildResult, String)),
    third: Option(Result(BuildResult, String)),
    finished: Bool,
  )
}

type JoinUpdate {
  First(Result(BuildResult, String))
  Second(Result(BuildResult, String))
  Third(Result(BuildResult, String))
}

fn record(
  context: dataflow.Context,
  state: dataflow.Mutation(JoinState),
  update: JoinUpdate,
  done: fn(Result(#(BuildResult, BuildResult, BuildResult), String)) -> Nil,
) -> Nil {
  let current =
    dataflow.value(dataflow.to_computation(dataflow.mutation(state)))
  let JoinState(first, second, third, finished) = current
  case finished {
    True -> Nil
    False -> {
      let next = case update {
        First(result) -> JoinState(Some(result), second, third, False)
        Second(result) -> JoinState(first, Some(result), third, False)
        Third(result) -> JoinState(first, second, Some(result), False)
      }
      let assert Ok(Nil) =
        dataflow.txn(context, fn() { dataflow.set(context, state, next) })
      complete_join(next, context, state, done)
    }
  }
}

fn complete_join(
  state: JoinState,
  context: dataflow.Context,
  mutation: dataflow.Mutation(JoinState),
  done: fn(Result(#(BuildResult, BuildResult, BuildResult), String)) -> Nil,
) -> Nil {
  let JoinState(first, second, third, _) = state
  case first, second, third {
    Some(Error(reason)), _, _ ->
      finish_join(context, mutation, done, Error(reason))
    _, Some(Error(reason)), _ ->
      finish_join(context, mutation, done, Error(reason))
    _, _, Some(Error(reason)) ->
      finish_join(context, mutation, done, Error(reason))
    Some(Ok(first)), Some(Ok(second)), Some(Ok(third)) ->
      finish_join(context, mutation, done, Ok(#(first, second, third)))
    _, _, _ -> Nil
  }
}

fn finish_join(
  context: dataflow.Context,
  mutation: dataflow.Mutation(JoinState),
  done: fn(Result(#(BuildResult, BuildResult, BuildResult), String)) -> Nil,
  result: Result(#(BuildResult, BuildResult, BuildResult), String),
) -> Nil {
  let current =
    dataflow.value(dataflow.to_computation(dataflow.mutation(mutation)))
  let JoinState(first, second, third, _) = current
  let assert Ok(Nil) =
    dataflow.txn(context, fn() {
      dataflow.set(context, mutation, JoinState(first, second, third, True))
    })
  done(result)
}

fn has_service_worker(entry_points: List(String)) -> Bool {
  case entry_points {
    [] -> False
    [first, ..rest] ->
      case
        string.ends_with(first, "svc_gateway_guest_run_service_worker_main.mjs")
      {
        True -> True
        False -> has_service_worker(rest)
      }
  }
}
