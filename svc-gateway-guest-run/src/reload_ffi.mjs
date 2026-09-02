import { Result$Error, Result$Ok } from '../prelude.mjs'
import {
  Option$None$const,
  Option$Some,
} from '../gleam_stdlib/gleam/option.mjs'

export function navigator() {
  return globalThis.navigator
}

export function window() {
  return globalThis.window
}

export function listen_for_message(navigator, onMessage) {
  navigator.serviceWorker.addEventListener('message', (event) => {
    onMessage(event.data)
  })
}

export function start_messages(navigator) {
  navigator.serviceWorker.startMessages()
}

export function platform(navigator) {
  return navigator.platform
}

export function listen_for_keydown(window, onKeydown) {
  window.addEventListener('keydown', (event) => {
    onKeydown(event.key, event.metaKey, event.ctrlKey, () =>
      event.preventDefault(),
    )
  })
}

export function get_registration(navigator, done) {
  navigator.serviceWorker
    .getRegistration()
    .then((registration) =>
      registration === undefined ?
        done(Result$Ok(Option$None$const))
      : done(Result$Ok(Option$Some(registration))),
    )
    .catch((error) => done(Result$Error(String(error))))
}

export function update_registration(registration, onUpdated, onError) {
  registration
    .update()
    .then(onUpdated)
    .catch((error) => onError(String(error)))
}

export function reload() {
  globalThis.location.reload()
}
