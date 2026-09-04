import { createServer } from 'node:http'
import { createSecureServer } from 'node:http2'
import { execFile } from 'node:child_process'
import { mkdtempSync, readFileSync } from 'node:fs'
import { readFile as readFileAsync, rm } from 'node:fs/promises'
import { tmpdir } from 'node:os'
import { resolve as resolvePath } from 'node:path'
import { join } from 'node:path'

import { List, Result$Error, Result$Ok, toBitArray } from '../prelude.mjs'
import {
  Option$None$const,
  Option$Some,
} from '../gleam_stdlib/gleam/option.mjs'

const { default: puppeteer } = await import(
  new URL(
    '../../../../../vendor/puppeteer-25.9.0/runtime/puppeteer.mjs',
    import.meta.url,
  )
)

function launch_options(args = []) {
  const executablePath = process.env.SYMBOLIZE_CHROMIUM_EXECUTABLE
  if (executablePath === undefined) {
    throw new Error('SYMBOLIZE_CHROMIUM_EXECUTABLE is not set')
  }
  return { args, executablePath, headless: true }
}

export function listen(handler, onError, done) {
  listenServer(createServer, handler, onError, done)
}

export function listen_secure(
  keyPath,
  certificatePath,
  handler,
  onError,
  done,
) {
  listenServer(
    (onRequest) =>
      createSecureServer(
        {
          allowHTTP1: true,
          cert: readFileSync(certificatePath),
          key: readFileSync(keyPath),
        },
        onRequest,
      ),
    handler,
    onError,
    done,
  )
}

function listenServer(create, handler, onError, done) {
  const server = create((request, response) => {
    try {
      const url = new URL(request.url ?? '/', 'http://127.0.0.1')
      handler(
        decodeURIComponent(url.pathname),
        url.search,
        request,
        response,
      )
    } catch (error) {
      onError(String(error))
      response.writeHead(500)
      response.end(String(error))
    }
  })
  server.listen(0, '127.0.0.1', () => {
    done(Result$Ok([server, server.address().port]))
  })
  server.on('error', (error) => done(Result$Error(String(error))))
}

export function certificate(done) {
  const directory = mkdtempSync(join(tmpdir(), 'symbolize-browser-'))
  const keyPath = join(directory, 'key.pem')
  const certificatePath = join(directory, 'certificate.pem')
  execFile(
    'openssl',
    [
      'req',
      '-x509',
      '-newkey',
      'rsa:2048',
      '-nodes',
      '-keyout',
      keyPath,
      '-out',
      certificatePath,
      '-days',
      '1',
      '-subj',
      '/CN=127.0.0.1',
    ],
    { stdio: 'ignore' },
    (error) =>
      done(
        error ?
          Result$Error(String(error))
        : Result$Ok([directory, keyPath, certificatePath]),
      ),
  )
}

export function remove_path(path, done) {
  rm(path, { force: true, recursive: true }).then(
    () => done(Result$Ok(undefined)),
    (error) => done(Result$Error(String(error))),
  )
}

export function close_server(server, done) {
  server.close((error) =>
    done(error ? Result$Error(String(error)) : Result$Ok(undefined)),
  )
}

export function read_file(path, done) {
  readFileAsync(path).then(
    (value) => done(Result$Ok(toBitArray([value]))),
    (error) => done(Result$Error(String(error))),
  )
}

export function read_request_body(request, onChunk, done) {
  request.on('data', (chunk) => onChunk(chunk.toString('utf8')))
  request.on('end', () => done(Result$Ok(undefined)))
  request.on('error', (error) => done(Result$Error(String(error))))
}

export function resolve_path(path) {
  return resolvePath(path)
}

export function reply(response, status, headers, body) {
  response.writeHead(status, Object.fromEntries(headers.toArray()))
  response.end(Buffer.from(body.rawBuffer))
}

export function start_response(response, status, headers) {
  response.writeHead(status, Object.fromEntries(headers.toArray()))
}

export function write(response, body) {
  response.write(body)
}

export function end(response, body) {
  response.end(body)
}

export function launch(done) {
  puppeteer.launch(launch_options()).then(
    (browser) => done(Result$Ok(browser)),
    (error) => done(Result$Error(String(error))),
  )
}

export function launch_insecure(done) {
  puppeteer.launch(launch_options(['--ignore-certificate-errors'])).then(
    (browser) => done(Result$Ok(browser)),
    (error) => done(Result$Error(String(error))),
  )
}

export function new_page(browser, onConsole, onError, onFailed, done) {
  browser.newPage().then(
    (page) => {
      page.on('console', (message) => onConsole(message.text()))
      page.on('pageerror', (error) => onError(String(error)))
      page.on('requestfailed', (request) => {
        onFailed(`${request.url()}: ${request.failure()?.errorText ?? ''}`)
      })
      done(Result$Ok(page))
    },
    (error) => done(Result$Error(String(error))),
  )
}

export function close_browser(browser, done) {
  browser.close().then(
    () => done(Result$Ok(undefined)),
    (error) => done(Result$Error(String(error))),
  )
}

export function goto(page, url, done) {
  page.goto(url).then(
    () => done(Result$Ok(undefined)),
    (error) => done(Result$Error(String(error))),
  )
}

export function reload(page, networkIdle, done) {
  page
    .reload(networkIdle ? { waitUntil: 'networkidle0' } : undefined)
    .then(
      () => done(Result$Ok(undefined)),
      (error) => done(Result$Error(String(error))),
    )
}

export function call_module(page, path, args, done) {
  page
    .evaluate(
      async ({ args, path }) => {
        const module = await import(path)
        await module.main(...args)
      },
      { args: args.toArray(), path },
    )
    .then(
      () => {
        done(Result$Ok(undefined))
      },
      (error) => {
        done(Result$Error(error?.stack ?? String(error)))
      },
    )
}

export function module_export_type(page, path, name, done) {
  page
    .evaluate(
      async ({ name, path }) => {
        const module = await import(path)
        return typeof module[name]
      },
      { name, path },
    )
    .then(
      (value) => done(Result$Ok(value)),
      (error) => done(Result$Error(String(error))),
    )
}

export function wait(milliseconds, done) {
  globalThis.setTimeout(() => {
    done()
  }, milliseconds)
}

export function fetch(page, url, headerNames, done) {
  page
    .evaluate(
      async ({ headerNames, url }) => {
        const response = await globalThis.fetch(url)
        return {
          body: await response.text(),
          headers: headerNames.map((name) => [
            name,
            response.headers.get(name) ?? '',
          ]),
          status: response.status,
        }
      },
      { headerNames: headerNames.toArray(), url },
    )
    .then(
      (result) =>
        done(
          Result$Ok([
            result.status,
            result.body,
            List.fromArray(result.headers),
          ]),
        ),
      (error) => done(Result$Error(String(error))),
    )
}

export function document_title(page, done) {
  page
    .evaluate(
      () => document.head.querySelector('title')?.textContent ?? '',
    )
    .then(done)
}

export function body_text(page, done) {
  page.evaluate(() => document.body.textContent ?? '').then(done)
}

export function body_children(page, done) {
  page
    .evaluate(() =>
      [...document.body.children].map((node) => node.tagName),
    )
    .then((value) => done(List.fromArray(value)))
}

function optional(value) {
  return value === null || value === undefined ?
      Option$None$const
    : Option$Some(value)
}

export function element_text(page, selector, done) {
  page
    .evaluate(
      (selector) => document.querySelector(selector)?.textContent ?? null,
      selector,
    )
    .then(
      (value) => done(Result$Ok(optional(value))),
      (error) => done(Result$Error(String(error))),
    )
}

export function element_attribute(page, selector, name, done) {
  page
    .evaluate(
      ({ name, selector }) =>
        document.querySelector(selector)?.getAttribute(name) ?? null,
      { name, selector },
    )
    .then(
      (value) => done(Result$Ok(optional(value))),
      (error) => done(Result$Error(String(error))),
    )
}

export function element_value(page, selector, done) {
  page
    .evaluate(
      (selector) => document.querySelector(selector)?.value ?? null,
      selector,
    )
    .then(
      (value) => done(Result$Ok(optional(value))),
      (error) => done(Result$Error(String(error))),
    )
}

export function element_checked(page, selector, done) {
  page
    .evaluate(
      (selector) => document.querySelector(selector)?.checked ?? null,
      selector,
    )
    .then(
      (value) => done(Result$Ok(optional(value))),
      (error) => done(Result$Error(String(error))),
    )
}

export function element_namespace(page, selector, done) {
  page
    .evaluate(
      (selector) => document.querySelector(selector)?.namespaceURI ?? null,
      selector,
    )
    .then(
      (value) => done(Result$Ok(optional(value))),
      (error) => done(Result$Error(String(error))),
    )
}

export function computed_style(page, selector, property, done) {
  page
    .evaluate(
      ({ property, selector }) => {
        const element = document.querySelector(selector)
        return element ?
            globalThis.getComputedStyle(element).getPropertyValue(property)
          : null
      },
      { property, selector },
    )
    .then(
      (value) => done(Result$Ok(optional(value))),
      (error) => done(Result$Error(String(error))),
    )
}

export function click(page, selector, done) {
  page.click(selector).then(
    () => done(Result$Ok(undefined)),
    (error) => done(Result$Error(String(error))),
  )
}

export function dispatch_keydown(page, key, meta, ctrl, done) {
  page
    .evaluate(
      ({ ctrl, key, meta }) => {
        const event = new KeyboardEvent('keydown', {
          bubbles: true,
          cancelable: true,
          ctrlKey: ctrl,
          key,
          metaKey: meta,
        })
        globalThis.window.dispatchEvent(event)
        return event.defaultPrevented
      },
      { ctrl, key, meta },
    )
    .then(
      (value) => done(Result$Ok(value)),
      (error) => done(Result$Error(String(error))),
    )
}

export function register_service_worker_module(page, path, scope, done) {
  page
    .evaluate(
      async ({ path, scope }) => {
        await navigator.serviceWorker.register(path, {
          scope,
          type: 'module',
        })
        await navigator.serviceWorker.ready
      },
      { path, scope },
    )
    .then(
      () => done(Result$Ok(undefined)),
      (error) => done(Result$Error(String(error))),
    )
}

export function register_service_worker_classic(page, path, scope, done) {
  page
    .evaluate(
      async ({ path, scope }) => {
        await navigator.serviceWorker.register(path, { scope })
        await navigator.serviceWorker.ready
      },
      { path, scope },
    )
    .then(
      () => done(Result$Ok(undefined)),
      (error) => done(Result$Error(String(error))),
    )
}

export function controller_state(page, done) {
  page
    .evaluate(
      () => globalThis.navigator.serviceWorker.controller?.state ?? 'none',
    )
    .then(
      (value) => done(Result$Ok(value)),
      (error) => done(Result$Error(String(error))),
    )
}

export function set_failure() {
  process.exitCode = 1
}
