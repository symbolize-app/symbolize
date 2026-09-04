import {
  Option$None$const,
  Option$Some,
} from '../gleam_stdlib/gleam/option.mjs'
import { Result$Error, Result$Ok, toList } from './gleam.mjs'

export function service_worker() {
  return globalThis.self
}

export function injected_version() {
  return Number(globalThis.version)
}

export function injected_manifest() {
  return toList(Object.entries(globalThis.manifest))
}

export function cache_promise(name) {
  return globalThis.self.caches.open(name)
}

export function listen_message(worker, onMessage) {
  worker.addEventListener('message', (event) =>
    onMessage(String(event.data ?? '')),
  )
}

export function listen_install(worker, onInstall) {
  worker.addEventListener('install', onInstall)
}

export function listen_activate(worker, onActivate) {
  worker.addEventListener('activate', onActivate)
}

export function listen_fetch(worker, onFetch) {
  worker.addEventListener('fetch', (event) => {
    const url = new URL(event.request.url)
    onFetch(
      event,
      url.origin === worker.location.origin,
      event.request.method,
      url.pathname,
    )
  })
}

export function skip_waiting(worker) {
  void worker.skipWaiting()
}

export function respond_with(event, produce) {
  event.respondWith(new Promise(produce))
}

export function new_request(url) {
  return new Request(url, { method: 'GET' })
}

export function fetch_request(request, onSuccess, onFailure) {
  globalThis
    .fetch(request)
    .then(onSuccess, (error) => onFailure(String(error)))
}

export function response_ok(response) {
  return response.ok
}

export function response_clone(response) {
  return response.clone()
}

export function response_with_headers(
  response,
  contentSecurityPolicy,
  contentType,
) {
  const clone = response.clone()
  return new Response(clone.body, {
    headers: {
      'content-security-policy': contentSecurityPolicy,
      'content-type': contentType,
    },
  })
}

export function response_text(body, contentSecurityPolicy, contentType) {
  return new Response(body, {
    headers: {
      'content-security-policy': contentSecurityPolicy,
      'content-type': contentType,
    },
  })
}

export function not_found(body) {
  return new Response(body, { status: 404 })
}

export function cache_match(cache, request, done) {
  cache
    .then((cache) => cache.match(request))
    .then(
      (response) =>
        done(
          Result$Ok(
            response === undefined ? Option$None$const : (
              Option$Some(response)
            ),
          ),
        ),
      (error) => done(Result$Error(String(error))),
    )
}

export function cache_put(cache, request, response, onSuccess, onFailure) {
  cache
    .then((cache) => cache.put(request, response))
    .then(onSuccess, (error) => onFailure(String(error)))
}

export function cache_keys(cache, onKey, onDone, onFailure) {
  cache
    .then((cache) => cache.keys())
    .then(
      (keys) => {
        onKey(toList(keys))
        onDone()
      },
      (error) => onFailure(String(error)),
    )
}

export function cache_delete(cache, request, onSuccess, onFailure) {
  cache
    .then((cache) => cache.delete(request))
    .then(onSuccess, (error) => onFailure(String(error)))
}

export function match_clients(worker, onClient, onDone, onFailure) {
  worker.clients.matchAll({ includeUncontrolled: true }).then(
    (clients) => {
      onClient(toList(clients))
      onDone()
    },
    (error) => onFailure(String(error)),
  )
}

export function post_message(client, value) {
  client.postMessage(value)
}

export function request_method(request) {
  return request.method
}

export function request_same_origin(request) {
  return new URL(request.url).origin === globalThis.self.location.origin
}

export function request_path(request) {
  return new URL(request.url).pathname
}

export function client_is_window(client) {
  return client instanceof WindowClient
}
