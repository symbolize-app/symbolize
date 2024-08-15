import contentSecurityPolicy from '@/contentSecurityPolicy.txt'
import fontCss from '@/font.css'
import loaderCss from '@/loader.css'
import mainHtml from '@/main.html'
import resetCss from '@/reset.css'
import * as collection from '@intertwine/lib-collection'
import * as time from '@intertwine/lib-time'
import * as timeBrowser from '@intertwine/lib-time/index.browser.ts'

declare const self: Readonly<ServiceWorkerGlobalScope>
declare const version: bigint
declare const manifest: Readonly<Record<string, string>>

const cacheName = 'code-v1'
const codeIdPrefix: Readonly<RegExp> = /^\/\.code\/\.id\//
const codePrefix: Readonly<RegExp> = /\/\.code\//

const cachePromise = self.caches.open(cacheName)

function main(): void {
  const ctx = { time: timeBrowser.time() }

  // eslint-disable-next-line no-console
  console.log(version, 'loading', self, manifest)

  self.addEventListener('message', (event) => {
    // eslint-disable-next-line no-console
    console.log(version, 'message', event)
  })

  self.addEventListener('install', () => {
    // eslint-disable-next-line no-console
    console.log(version, 'install')
    void self.skipWaiting()
  })

  self.addEventListener('activate', () => {
    // eslint-disable-next-line no-console
    console.log(version, 'activate')
    void resetClients()
    void prepareCache(ctx)
  })

  self.addEventListener('fetch', (event) => {
    const url = new URL(event.request.url)
    if (url.origin === self.origin && event.request.method === 'GET') {
      handle(event, url)
    }
  })
}

function handle(event: Readonly<FetchEvent>, url: Readonly<URL>): void {
  const path = url.pathname
  const codeIdPath = collection.stripPrefix(path, codeIdPrefix)
  if (codeIdPath) {
    event.respondWith(fetchContentById(codeIdPath))
  } else {
    const codePath = collection.stripPrefix(path, codePrefix)
    if (codePath) {
      event.respondWith(fetchContentByPath(codePath))
    } else {
      event.respondWith(patchMainHtmlContent())
    }
  }
}

async function fetchContentByPath(codePath: string): Promise<Response> {
  const codeIdPath = manifest[codePath] ?? null
  if (!codeIdPath) {
    // eslint-disable-next-line no-console
    console.error(version, 'manifest error', codePath)
    return new Response('Path missing from manifest', { status: 404 })
  } else {
    return fetchContentById(codeIdPath)
  }
}

const cssImportPattern = /@import url\('(?<parameterName>[^.]*).css'\);/g

function patchMainHtmlContent(): Response {
  const body = collection.applyTemplate(cssImportPattern, mainHtml, {
    font: fontCss,
    loader: loaderCss,
    reset: resetCss,
  })
  return new Response(body, {
    headers: buildHeaders('main.html'),
  })
}

const fetchContentMemo = collection.memo(
  async (codeIdPath: string): Promise<Response> => {
    let ok = false
    try {
      const cache = await cachePromise
      const request = new Request(`/.code/.id/${codeIdPath}`)
      const cacheResponse = await cache.match(request)
      if (cacheResponse) {
        return cacheResponse
      } else {
        // eslint-disable-next-line no-console
        console.log(version, 'cache miss', codeIdPath)
        const response = await fetch(request)
        ok = response.ok
        if (ok) {
          await cache.put(request, response.clone())
        }
        return response
      }
    } finally {
      if (!ok) {
        fetchContentMemo.delete(codeIdPath)
      }
    }
  },
)

async function fetchContentById(codeIdPath: string): Promise<Response> {
  // Make sure to clone the response to allow a new read, and create a
  // brand new response to reset the relative path
  const response = (await fetchContentMemo.get(codeIdPath)).clone()
  return new Response(response.body, {
    headers: buildHeaders(codeIdPath),
  })
}

function buildHeaders(path: string): Record<string, string> {
  let contentType: string
  if (path.endsWith('.html')) {
    contentType = 'text/html'
  } else if (path.endsWith('.js') || path.endsWith('mjs')) {
    contentType = 'text/javascript'
  } else if (path.endsWith('.woff2')) {
    contentType = 'font/woff2'
  } else {
    throw new Error(`Unknown content type for ${path}`)
  }
  return {
    ['content-security-policy']: contentSecurityPolicy.trimEnd(),
    ['content-type']: contentType,
  }
}

async function prepareCache(ctx: time.Context): Promise<void> {
  const cache = await cachePromise

  await time.delay(ctx, 5_000)

  const contentPaths = new Set(Object.values(manifest))

  for (const key of await cache.keys()) {
    const url = new URL(key.url)
    const contentPath = collection.stripPrefix(url.pathname, codeIdPrefix)
    if (
      url.origin !== self.origin ||
      key.method !== 'GET' ||
      !contentPath ||
      !contentPaths.delete(contentPath)
    ) {
      // eslint-disable-next-line no-console
      console.log(version, 'evict', key.method, key.url)
      await cache.delete(key)
    }
  }

  for (const contentPath of contentPaths) {
    // eslint-disable-next-line no-console
    console.log(version, 'prepare', contentPath)
    await fetchContentMemo.get(contentPath)
  }
}

async function resetClients(): Promise<void> {
  const clients = await self.clients.matchAll({
    includeUncontrolled: true,
  })
  for (const client of clients) {
    if (client instanceof WindowClient) {
      // eslint-disable-next-line no-console
      console.log(version, 'reloading window', client.id)
      client.postMessage('reload')
    }
  }
}

main()

export {}
