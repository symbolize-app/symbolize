import gleam/dict.{type Dict}
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/set.{type Set}
import gleam/string

pub type Context {
  Context(outbase: String, pnpm_package_versions: Dict(String, String))
}

pub type Resolver {
  Resolver(
    context: Context,
    all_entry_points: Set(String),
    new_entry_points: List(String),
  )
}

pub fn context(outbase: String) -> Context {
  Context(outbase, dict.new())
}

pub fn resolver(context: Context, entry_points: List(String)) -> Resolver {
  Resolver(context, set.from_list(entry_points), [])
}

pub fn resolver_context(resolver: Resolver) -> Context {
  resolver.context
}

pub fn resolver_new_entry_points(resolver: Resolver) -> List(String) {
  resolver.new_entry_points
}

pub fn take_new_entry_points(resolver: Resolver) -> #(Resolver, List(String)) {
  #(
    Resolver(resolver.context, resolver.all_entry_points, []),
    resolver.new_entry_points,
  )
}

pub fn output_entry_points(
  resolver: Resolver,
  input_paths: List(String),
) -> Result(#(Resolver, List(#(String, String))), String) {
  output_entry_points_loop(resolver, input_paths, [])
}

fn output_entry_points_loop(
  resolver: Resolver,
  input_paths: List(String),
  output_paths: List(#(String, String)),
) -> Result(#(Resolver, List(#(String, String))), String) {
  case input_paths {
    [] -> Ok(#(resolver, output_paths))
    [first, ..rest] ->
      case convert_to_out_path(resolver.context, first) {
        Error(reason) -> Error(reason)
        Ok(#(context, output_path)) -> {
          let resolver =
            Resolver(
              context,
              resolver.all_entry_points,
              resolver.new_entry_points,
            )
          output_entry_points_loop(
            resolver,
            rest,
            append(output_paths, [#(first, output_path)]),
          )
        }
      }
  }
}

pub fn rewrite_resolved_path(
  resolver: Resolver,
  importer: String,
  resolved_path: String,
) -> Result(#(Resolver, String), String) {
  let Resolver(context, all_entry_points, new_entry_points) = resolver
  use #(context, out_path) <- result.try(convert_to_out_path(
    context,
    resolved_path,
  ))
  use #(context, importer_out_path) <- result.try(convert_to_out_path(
    context,
    importer,
  ))
  let new_entry_points = case set.contains(all_entry_points, resolved_path) {
    True -> new_entry_points
    False -> append(new_entry_points, [resolved_path])
  }
  let all_entry_points = set.insert(all_entry_points, resolved_path)
  let relative_path = relative_import(importer_out_path, out_path) <> ".mjs"
  Ok(#(Resolver(context, all_entry_points, new_entry_points), relative_path))
}

pub fn convert_to_out_path(
  context: Context,
  input_path: String,
) -> Result(#(Context, String), String) {
  let relative = relative_path(context.outbase, input_path)
  case generated_gateway_entry(relative) {
    Some(output_path) -> Ok(#(context, output_path))
    None ->
      case generated_development_entry(relative) {
        Some(output_path) -> Ok(#(context, output_path))
        None ->
          case generated_module_output_path(relative) {
            Some(output_path) -> Ok(#(context, output_path))
            None -> convert_regular_out_path(context, relative)
          }
      }
  }
}

fn convert_regular_out_path(
  context: Context,
  relative: String,
) -> Result(#(Context, String), String) {
  case pnpm_match(relative) {
    None -> Ok(#(context, relative))
    Some(#(package, version, suffix)) -> {
      case dict.get(context.pnpm_package_versions, package) {
        Error(Nil) ->
          Ok(#(
            Context(
              context.outbase,
              dict.insert(context.pnpm_package_versions, package, version),
            ),
            ".pnpm-" <> package <> "/" <> suffix,
          ))
        Ok(previous) if previous == version ->
          Ok(#(context, ".pnpm-" <> package <> "/" <> suffix))
        Ok(previous) ->
          Error(
            "Ambiguous versions found for "
            <> package
            <> ": "
            <> previous
            <> " / "
            <> version,
          )
      }
    }
  }
}

fn generated_gateway_entry(path: String) -> Option(String) {
  case
    string.ends_with(path, "/svc_gateway_guest_run_dedicated_worker_main.mjs")
  {
    True -> Some("svc-gateway-guest-run/dedicatedWorker")
    False ->
      case string.ends_with(path, "/svc_gateway_guest_run_main.mjs") {
        True -> Some("svc-gateway-guest-run/main")
        False ->
          case string.ends_with(path, "/svc_gateway_guest_run_register.mjs") {
            True -> Some("svc-gateway-guest-run/serviceWorkerRegister")
            False ->
              case
                string.ends_with(
                  path,
                  "/svc_gateway_guest_run_service_worker_main.mjs",
                )
              {
                True -> Some("svc-gateway-guest-run/serviceWorker")
                False -> None
              }
          }
      }
  }
}

fn generated_development_entry(path: String) -> Option(String) {
  case string.ends_with(path, "/svc_gateway_guest_run_development.mjs") {
    True -> Some("svc-gateway-guest-run/main.development")
    False -> None
  }
}

// Gleam compiles a package into a package-local JavaScript tree. That tree is
// an implementation detail, not the URL layout used by the original build:
// publishing it would expose paths such as build/dev/javascript/.../foo.mjs.mjs.
// Keep the compiler output as the esbuild input, but give each generated module
// a stable logical output path before imports are rewritten.
fn generated_module_output_path(path: String) -> Option(String) {
  case string.split(path, on: "/") {
    [_, "build", _, "javascript", "prelude.mjs"] -> Some(".gleam/prelude")
    [_, "build", _, "javascript", package, ..module_parts] ->
      generated_package_output_path(package, module_parts)
    _ -> None
  }
}

fn generated_package_output_path(
  package: String,
  module_parts: List(String),
) -> Option(String) {
  case generated_package_prefix(package) {
    None -> None
    Some(prefix) ->
      case module_parts {
        [module] ->
          case generated_source_module(package, strip_mjs(module)) {
            Some(output_path) -> Some(output_path)
            None ->
              Some(
                prefix
                <> generated_private_separator(package)
                <> strip_mjs(module),
              )
          }
        _ ->
          Some(
            prefix
            <> generated_private_separator(package)
            <> join("__", strip_mjs_list(module_parts)),
          )
      }
  }
}

fn generated_private_separator(package: String) -> String {
  case package {
    "gleam_stdlib" -> "/"
    _ -> "/.gleam/"
  }
}

fn generated_package_prefix(package: String) -> Option(String) {
  case package {
    "gleam_stdlib" -> Some(".gleam/gleam_stdlib")
    "symbolize_lib_collection" -> Some("lib-collection")
    "symbolize_lib_dataflow" -> Some("lib-dataflow")
    "symbolize_lib_error" -> Some("lib-error")
    "symbolize_lib_hex" -> Some("lib-hex")
    "symbolize_lib_markup" -> Some("lib-markup")
    "symbolize_lib_payload" -> Some("lib-payload")
    "symbolize_lib_random" -> Some("lib-random")
    "symbolize_lib_stream" -> Some("lib-stream")
    "symbolize_lib_styling" -> Some("lib-styling")
    "symbolize_lib_time" -> Some("lib-time")
    "symbolize_svc_auth_guest_read" -> Some("svc-auth-guest-read")
    "symbolize_svc_auth_guest_view" -> Some("svc-auth-guest-view")
    "symbolize_svc_gateway_guest_run" -> Some("svc-gateway-guest-run")
    _ -> None
  }
}

fn generated_source_module(package: String, module: String) -> Option(String) {
  case package, module {
    "gleam_stdlib", "gleam" -> Some(".gleam/gleam_stdlib/runtime")
    "gleam_stdlib", "gleam_stdlib" -> Some(".gleam/gleam_stdlib/stdlib")
    "symbolize_lib_collection", "lib_collection" -> Some("lib-collection/index")
    "symbolize_lib_dataflow", "lib_dataflow" -> Some("lib-dataflow/index")
    "symbolize_lib_error", "lib_error" -> Some("lib-error/index")
    "symbolize_lib_hex", "lib_hex" -> Some("lib-hex/index")
    "symbolize_lib_markup", "lib_markup_attributes" ->
      Some("lib-markup/elementAttr")
    "symbolize_lib_markup", "lib_markup_conditional" -> Some("lib-markup/if_")
    "symbolize_lib_markup", "lib_markup_context" -> Some("lib-markup/context")
    "symbolize_lib_markup", "lib_markup_custom" -> Some("lib-markup/custom")
    "symbolize_lib_markup", "lib_markup_data" -> Some("lib-markup/data")
    "symbolize_lib_markup", "lib_markup_dom" -> Some("lib-markup/element")
    "symbolize_lib_markup", "lib_markup_each" -> Some("lib-markup/each")
    "symbolize_lib_markup", "lib_markup_fragment" -> Some("lib-markup/fragment")
    "symbolize_lib_markup", "lib_markup_html" -> Some("lib-markup/html")
    "symbolize_lib_markup", "lib_markup_math" -> Some("lib-markup/math")
    "symbolize_lib_markup", "lib_markup_scheduler" ->
      Some("lib-markup/scheduler")
    "symbolize_lib_markup", "lib_markup_select" -> Some("lib-markup/select")
    "symbolize_lib_markup", "lib_markup_svg" -> Some("lib-markup/svg")
    "symbolize_lib_payload", "lib_payload" -> Some("lib-payload/index")
    "symbolize_lib_random", "lib_random" -> Some("lib-random/index")
    "symbolize_lib_stream", "lib_stream" -> Some("lib-stream/index")
    "symbolize_lib_stream", "lib_stream_context" -> Some("lib-stream/context")
    "symbolize_lib_stream", "lib_stream_http" -> Some("lib-stream/http/client")
    "symbolize_lib_stream", "lib_stream_sink" -> Some("lib-stream/sink")
    "symbolize_lib_stream", "lib_stream_source" -> Some("lib-stream/source")
    "symbolize_lib_time", "lib_time" -> Some("lib-time/index")
    "symbolize_lib_styling", "lib_styling" -> Some("lib-styling/index")
    "symbolize_lib_styling", "lib_styling_atom" -> Some("lib-styling/atom")
    "symbolize_lib_styling", "lib_styling_context" ->
      Some("lib-styling/context")
    "symbolize_lib_styling", "lib_styling_data" ->
      Some("lib-styling/data/index")
    "symbolize_lib_styling", "lib_styling_expr" ->
      Some("lib-styling/expression")
    "symbolize_lib_styling", "lib_styling_gradient" ->
      Some("lib-styling/data/gradient")
    "symbolize_lib_styling", "lib_styling_media" -> Some("lib-styling/media")
    "symbolize_lib_styling", "lib_styling_select" -> Some("lib-styling/select")
    "symbolize_lib_styling", "lib_styling_support" ->
      Some("lib-styling/support")
    "symbolize_lib_styling", "lib_styling_var" -> Some("lib-styling/var_")
    "symbolize_svc_auth_guest_read", "svc_auth_guest_read_main" ->
      Some("svc-auth-guest-read/main")
    "symbolize_svc_auth_guest_view", "svc_auth_guest_view_main" ->
      Some("svc-auth-guest-view/main")
    "symbolize_svc_gateway_guest_run", "svc_gateway_guest_run_reload" ->
      Some("svc-gateway-guest-run/reload")
    _, _ -> None
  }
}

fn strip_mjs(value: String) -> String {
  case string.ends_with(value, ".mjs") {
    True -> string.drop_end(value, 4)
    False -> value
  }
}

fn strip_mjs_list(values: List(String)) -> List(String) {
  case values {
    [] -> []
    [first, ..rest] -> [strip_mjs(first), ..strip_mjs_list(rest)]
  }
}

pub fn relative_import(
  from_output_path: String,
  to_output_path: String,
) -> String {
  let from_directory = directory(from_output_path)
  let relative = relative_path(from_directory, to_output_path)
  case string.starts_with(relative, ".gleam/") {
    True -> "./" <> relative
    False ->
      case string.starts_with(relative, ".") {
        True -> relative
        False -> "./" <> relative
      }
  }
}

fn pnpm_match(relative: String) -> Option(#(String, String, String)) {
  let prefix = "node_modules/.pnpm/"
  case string.starts_with(relative, prefix) {
    False -> None
    True -> {
      let rest = string.drop_start(relative, string.length(prefix))
      case string.split_once(rest, on: "/") {
        Error(Nil) -> None
        Ok(#(package_version, after_package)) ->
          case split_package_version(package_version) {
            None -> None
            Some(#(package, version)) ->
              case string.starts_with(after_package, "node_modules/") {
                False -> None
                True -> {
                  let dependency_path =
                    string.drop_start(
                      after_package,
                      string.length("node_modules/"),
                    )
                  case string.split_once(dependency_path, on: "/") {
                    Error(Nil) -> None
                    Ok(#(_, suffix)) -> Some(#(package, version, suffix))
                  }
                }
              }
          }
      }
    }
  }
}

fn split_package_version(value: String) -> Option(#(String, String)) {
  case string.split_once(string.reverse(value), on: "@") {
    Error(Nil) -> None
    Ok(#(reversed_version, reversed_package)) ->
      Some(#(string.reverse(reversed_package), string.reverse(reversed_version)))
  }
}

fn relative_path(from: String, to: String) -> String {
  let from_parts = components(from)
  let to_parts = components(to)
  let common = common_length(from_parts, to_parts, 0)
  let parents = repeat("..", list_length(from_parts) - common, [])
  let children = drop(to_parts, common)
  join("/", append(parents, children))
}

fn directory(path: String) -> String {
  case string.split_once(string.reverse(path), on: "/") {
    Error(Nil) -> "."
    Ok(#(_, reversed_directory)) -> string.reverse(reversed_directory)
  }
}

fn components(path: String) -> List(String) {
  case path {
    "." -> []
    _ ->
      case string.starts_with(path, "/") {
        True -> ["", ..string.split(string.drop_start(path, 1), on: "/")]
        False -> string.split(path, on: "/")
      }
  }
}

fn common_length(first: List(String), second: List(String), count: Int) -> Int {
  case first, second {
    [first_head, ..first_tail], [second_head, ..second_tail]
      if first_head == second_head
    -> common_length(first_tail, second_tail, count + 1)
    _, _ -> count
  }
}

fn repeat(value: String, count: Int, output: List(String)) -> List(String) {
  case count <= 0 {
    True -> output
    False -> repeat(value, count - 1, [value, ..output])
  }
}

fn drop(values: List(a), count: Int) -> List(a) {
  case count <= 0 {
    True -> values
    False ->
      case values {
        [] -> []
        [_, ..rest] -> drop(rest, count - 1)
      }
  }
}

fn list_length(values: List(a)) -> Int {
  case values {
    [] -> 0
    [_, ..rest] -> 1 + list_length(rest)
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
