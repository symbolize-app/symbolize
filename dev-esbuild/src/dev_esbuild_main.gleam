import dev_esbuild_build as build
import dev_esbuild_context as dev_context
import dev_esbuild_db as db
import dev_esbuild_modules as modules
import dev_esbuild_node as node
import dev_esbuild_output as output
import gleam/float
import gleam/int
import gleam/io
import gleam/string
import lib_time

pub type Arguments {
  Arguments(clean: Bool, mode: dev_context.Mode)
}

pub fn main() {
  case parse_arguments(node.argv()) {
    Error(reason) -> fail(reason)
    Ok(arguments) -> run(arguments)
  }
}

fn run(arguments: Arguments) -> Nil {
  let Arguments(clean, mode) = arguments
  let outdir = node.resolve("../build/guest/" <> mode_name(mode))
  case clean {
    True -> node.remove(outdir)(fn(result) { report_result(result) })
    False -> {
      db.init()(fn(result) {
        case result {
          Error(reason) -> fail(reason)
          Ok(database) -> {
            let time = lib_time.new_context(lib_time.time())
            let context =
              dev_context.context(dev_context.dev(mode, outdir), database, time)
            node.mkdir(outdir)(fn(result) {
              case result {
                Error(reason) -> fail(reason)
                Ok(Nil) -> build(context)
              }
            })
          }
        }
      })
    }
  }
}

fn build(context: dev_context.Context) -> Nil {
  let mode = dev_context.mode(dev_context.dev_context(context))
  let production = case mode {
    dev_context.Development -> False
    dev_context.Production -> True
  }
  let outbase = node.resolve("..")
  let copy_entry_points =
    list_map(
      [
        "../svc-gateway-guest-run/.font/literata-italic.woff2",
        "../svc-gateway-guest-run/.font/literata.woff2",
        "../svc-gateway-guest-run/init.html",
      ],
      node.resolve,
    )
  let classic_entry_points =
    list_map(
      [
        "../svc-gateway-guest-run/build/dev/javascript/symbolize_svc_gateway_guest_run/svc_gateway_guest_run_service_worker_main.mjs",
      ],
      node.resolve,
    )
  let module_entry_points =
    list_map(
      case production {
        True -> [
          "../svc-gateway-guest-run/build/dev/javascript/symbolize_svc_gateway_guest_run/svc_gateway_guest_run_dedicated_worker_main.mjs",
          "../svc-gateway-guest-run/build/dev/javascript/symbolize_svc_gateway_guest_run/svc_gateway_guest_run_main.mjs",
          "../svc-gateway-guest-run/build/dev/javascript/symbolize_svc_gateway_guest_run/svc_gateway_guest_run_register.mjs",
        ]
        False -> [
          "../svc-gateway-guest-run/build/dev/javascript/symbolize_svc_gateway_guest_run/svc_gateway_guest_run_dedicated_worker_main.mjs",
          "../svc-gateway-guest-run/build/dev/javascript/symbolize_svc_gateway_guest_run/svc_gateway_guest_run_main.mjs",
          "../svc-gateway-guest-run/build/dev/javascript/symbolize_svc_gateway_guest_run/svc_gateway_guest_run_register.mjs",
          "../svc-gateway-guest-run/build/dev/javascript/symbolize_svc_gateway_guest_run/svc_gateway_guest_run_development.mjs",
        ]
      },
      node.resolve,
    )
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

fn report_result(result: Result(Nil, String)) -> Nil {
  case result {
    Ok(Nil) -> Nil
    Error(reason) -> {
      fail(reason)
    }
  }
}

fn parse_arguments(arguments: List(String)) -> Result(Arguments, String) {
  parse_arguments_loop(arguments, False, dev_context.Development, False)
}

fn parse_arguments_loop(
  arguments: List(String),
  clean: Bool,
  mode: dev_context.Mode,
  mode_seen: Bool,
) -> Result(Arguments, String) {
  case arguments {
    [] -> Ok(Arguments(clean, mode))
    ["--clean", ..rest] -> parse_arguments_loop(rest, True, mode, mode_seen)
    ["--mode", value, ..rest] ->
      case mode_seen {
        True -> Error("Option '--mode' was specified more than once")
        False ->
          case string.starts_with(value, "-") {
            True ->
              Error(
                "Option '--mode' argument is ambiguous. Did you forget to "
                <> "specify the option argument for '--mode'?",
              )
            False ->
              case parse_mode(value) {
                Error(reason) -> Error(reason)
                Ok(mode) -> parse_arguments_loop(rest, clean, mode, True)
              }
          }
      }
    ["--mode"] -> Error("Option '--mode <value>' argument missing")
    ["--", ..rest] ->
      case rest {
        [] -> Ok(Arguments(clean, mode))
        [first, ..] ->
          Error(
            "Unexpected argument '"
            <> first
            <> "'. This command does not take positional arguments",
          )
      }
    [argument, ..rest] ->
      case string.starts_with(argument, "--mode=") {
        True ->
          case mode_seen {
            True -> Error("Option '--mode' was specified more than once")
            False ->
              case parse_mode(string.drop_start(argument, 7)) {
                Error(reason) -> Error(reason)
                Ok(mode) -> parse_arguments_loop(rest, clean, mode, True)
              }
          }
        False ->
          case string.starts_with(argument, "--clean=") {
            True -> Error("Option '--clean' does not take an argument")
            False ->
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
  }
}

fn parse_mode(value: String) -> Result(dev_context.Mode, String) {
  case value {
    "development" -> Ok(dev_context.Development)
    "production" -> Ok(dev_context.Production)
    _ ->
      Error(
        "Invalid string option (not \"development\" | \"production\") "
        <> "at (root)",
      )
  }
}

fn mode_name(mode: dev_context.Mode) -> String {
  case mode {
    dev_context.Development -> "development"
    dev_context.Production -> "production"
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
