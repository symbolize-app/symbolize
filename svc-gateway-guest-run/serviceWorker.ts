import * as collection from '@intertwine/lib-collection'

declare const self: ServiceWorkerGlobalScope
declare const version: bigint
declare const manifest: Record<string, string>

const cacheName = 'code-v1'
const codeIdPrefix = /^\/\.code\/\.id\//
const codePrefix = /\/\.code\//
const mainHtmlPath = 'svc-gateway-guest-run/main.html'

function main(): void {
  console.log(version, 'loading', self, manifest)

  self.addEventListener('message', (event) => {
    console.log(version, 'message:', event)
  })

  self.addEventListener('install', (event) => {
    console.log(version, 'installing...')

    prepareCache()

    event.waitUntil(self.skipWaiting())
  })

  self.addEventListener('activate', () => {
    console.log(version, 'activating...')

    void (async () => {
      const clients = new Set(
        await self.clients.matchAll({
          includeUncontrolled: true,
        })
      )
      for (const client of await self.clients.matchAll()) {
        clients.delete(client)
      }
      console.log(version, 'clients:', clients)
      for (const client of clients) {
        client.postMessage(['TEST0', version])
      }
    })()

    void cleanCache()
  })

  self.addEventListener('fetch', (event) => {
    const url = new URL(event.request.url)
    if (url.origin === self.origin && event.request.method === 'GET') {
      handle(event, url)
    }
  })
}

function handle(event: FetchEvent, url: URL) {
  const path = url.pathname
  const codeIdPath = collection.stripPrefix(path, codeIdPrefix)
  const codePath = codeIdPath ?? collection.stripPrefix(path, codePrefix)
  if (codeIdPath) {
    handleContentById(event, codeIdPath, true)
  } else if (codePath) {
    handleContentByPath(event, codePath, true)
  } else {
    handleContentByPath(event, mainHtmlPath, false)
  }
}

function handleContentById(
  event: FetchEvent,
  contentPath: string,
  sandbox: boolean
) {
  event.respondWith(patchFetchContent(contentPath, sandbox))
}

function handleContentByPath(
  event: FetchEvent,
  path: string,
  sandbox: boolean
) {
  const contentPath = manifest[path] ?? null
  if (!contentPath) {
    console.error(version, 'manifest error', path)
    event.respondWith(
      new Response('Path missing from manifest', { status: 404 })
    )
  } else {
    handleContentById(event, contentPath, sandbox)
  }
}

const fetchContentMemo = collection.memo(
  async (contentPath: string): Promise<Response> => {
    let ok = false
    try {
      const cache = await self.caches.open(cacheName)
      const request = new Request(`/.code/.id/${contentPath}`)
      const cacheResponse = await cache.match(request)
      if (cacheResponse) {
        return cacheResponse
      } else {
        console.log(version, 'cache miss', contentPath)
        const response = await fetch(request)
        ok = response.ok
        if (ok) {
          await cache.put(request, response.clone())
        }
        return response
      }
    } finally {
      if (!ok) {
        fetchContentMemo.delete(contentPath)
      }
    }
  }
)

async function patchFetchContent(
  contentPath: string,
  sandbox: boolean
): Promise<Response> {
  const response = (await fetchContentMemo.get(contentPath)).clone()
  const headers = new Headers(response.headers)
  const contentSecurityPolicy = headers.get('content-security-policy')
  if (!sandbox && contentSecurityPolicy) {
    headers.set(
      'content-security-policy',
      contentSecurityPolicy
        .split(';')
        .filter((item) => item !== 'sandbox')
        .join(';')
    )
  }
  return new Response(response.body, {
    headers,
    status: response.status,
    statusText: response.statusText,
  })
}

function prepareCache() {
  for (const contentPath of Object.values(manifest)) {
    console.log(version, 'prepare', contentPath)
    void fetchContentMemo.get(contentPath)
  }
}

async function cleanCache() {
  const cache = await self.caches.open(cacheName)
  const contentPaths = new Set(Object.values(manifest))
  for (const key of await cache.keys()) {
    const codePath = collection.stripPrefix(
      new URL(key.url).pathname,
      codeIdPrefix
    )
    if (!codePath || !contentPaths.has(codePath)) {
      await cache.delete(key)
    }
  }
}

main()

export {}
