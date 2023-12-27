declare const self: ServiceWorkerGlobalScope
declare const version: bigint
declare const manifest: Record<string, string>

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

    event.waitUntil(
      Promise.all([
        self.skipWaiting(),
        self.caches.open('static-v1').then((cache) => cache.add('/')),
      ])
    )
  })

  self.addEventListener('activate', (event) => {
    console.log(version, 'activating...')

    event.waitUntil(
      (async () => {
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
    )
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
  const codeIdPath = stripPrefix(path, codeIdPrefix)
  const codePath = codeIdPath ?? stripPrefix(path, codePrefix)
  if (codeIdPath) {
    handleContentById(event, event.request, true)
  } else if (codePath) {
    handleContentByPath(event, codePath, true)
  } else {
    handleContentByPath(event, mainHtmlPath, false)
  }
}

function handleContentById(
  event: FetchEvent,
  request: Request,
  sandbox: boolean
) {
  event.respondWith(fetchContent(request, sandbox))
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
    handleContentById(
      event,
      new Request(`${self.origin}/.code/.id/${contentPath}`, {
        ...event.request,
        mode: 'cors',
      }),
      sandbox
    )
  }
}

async function fetchContent(
  request: Request,
  sandbox: boolean
): Promise<Response> {
  const response = await fetch(request)
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
  return new Response(response.body, { ...response, headers })
}

function stripPrefix(input: string, pattern: RegExp): string | null {
  const match = pattern.exec(input)
  if (match) {
    return input.substring(match[0].length)
  } else {
    return null
  }
}

main()

export {}
