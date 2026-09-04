import gleam/option.{type Option, None, Some}
import gleam/string
import lib_collection

const code_id_prefix = "/.code/.id/"

const code_prefix = "/.code/"

pub type Route {
  ContentById(String)
  ContentByPath(String)
  MainHtml
}

pub fn route(path: String) -> Route {
  case
    non_empty(lib_collection.strip_prefix(
      path,
      lib_collection.prefix(code_id_prefix),
    ))
  {
    Some(content_id) -> ContentById(content_id)
    None ->
      case
        non_empty(lib_collection.strip_prefix(
          path,
          lib_collection.prefix(code_prefix),
        ))
      {
        Some(content_path) -> ContentByPath(content_path)
        None -> MainHtml
      }
  }
}

pub fn content_type(path: String) -> Result(String, String) {
  case string.ends_with(path, ".html") {
    True -> Ok("text/html")
    False ->
      case string.ends_with(path, ".js") || string.ends_with(path, "mjs") {
        True -> Ok("text/javascript")
        False ->
          case string.ends_with(path, ".woff2") {
            True -> Ok("font/woff2")
            False -> Error("Unknown content type for " <> path)
          }
      }
  }
}

pub fn headers(
  content_security_policy: String,
  path: String,
) -> Result(List(#(String, String)), String) {
  case content_type(path) {
    Error(reason) -> Error(reason)
    Ok(value) ->
      Ok([
        #("content-security-policy", string.trim_end(content_security_policy)),
        #("content-type", value),
      ])
  }
}

fn non_empty(value: Option(String)) -> Option(String) {
  case value {
    Some("") -> None
    value -> value
  }
}
