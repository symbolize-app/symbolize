import gleam/io
import gleam/option.{type Option, None, Some}
import gleam/string

pub type Navigator

pub type Window

pub type Registration

@external(javascript, "./reload_ffi.mjs", "navigator")
fn navigator() -> Navigator

@external(javascript, "./reload_ffi.mjs", "window")
fn window() -> Window

@external(javascript, "./reload_ffi.mjs", "listen_for_message")
fn listen_for_message_ffi(
  navigator: Navigator,
  on_message: fn(String) -> Nil,
) -> Nil

@external(javascript, "./reload_ffi.mjs", "start_messages")
fn start_messages(navigator: Navigator) -> Nil

@external(javascript, "./reload_ffi.mjs", "platform")
fn platform(navigator: Navigator) -> String

@external(javascript, "./reload_ffi.mjs", "listen_for_keydown")
fn listen_for_keydown_ffi(
  window: Window,
  on_keydown: fn(String, Bool, Bool, fn() -> Nil) -> Nil,
) -> Nil

@external(javascript, "./reload_ffi.mjs", "get_registration")
fn get_registration_ffi(
  navigator: Navigator,
  done: fn(Result(Option(Registration), String)) -> Nil,
) -> Nil

@external(javascript, "./reload_ffi.mjs", "update_registration")
fn update_registration_ffi(
  registration: Registration,
  on_updated: fn() -> Nil,
  on_error: fn(String) -> Nil,
) -> Nil

@external(javascript, "./reload_ffi.mjs", "reload")
fn reload() -> Nil

pub fn listen_for_message() -> Nil {
  let navigator = navigator()
  listen_for_message_ffi(navigator, fn(data) {
    case data {
      "reload" -> {
        io.println("reload")
        reload()
      }
      _ -> Nil
    }
  })
  start_messages(navigator)
}

pub fn listen_for_keyboard_shortcut() -> Nil {
  let navigator = navigator()
  let is_mac =
    navigator
    |> platform
    |> string.lowercase
    |> string.starts_with("mac")
  listen_for_keydown_ffi(window(), fn(key, meta, control, prevent_default) {
    let modifier = case is_mac {
      True -> meta
      False -> control
    }
    case key == "s" && modifier {
      True -> {
        update(navigator)
        prevent_default()
      }
      False -> Nil
    }
  })
}

fn update(navigator: Navigator) -> Nil {
  io.println("update")
  get_registration_ffi(navigator, fn(result) {
    case result {
      Error(reason) -> panic as reason
      Ok(None) -> panic as "Missing service worker registration"
      Ok(Some(registration)) ->
        update_registration_ffi(registration, fn() { Nil }, fn(reason) {
          panic as reason
        })
    }
  })
}
