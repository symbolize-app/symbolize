function main(self: ServiceWorkerGlobalScope): void {
  const version = 2

  console.log(version, 'loading', self)

  self.addEventListener('message', (event) => {
    console.log(version, 'message:', event)
  })

  self.addEventListener('install', (event) => {
    console.log(version, 'installing...')

    event.waitUntil(
      Promise.all([
        self.skipWaiting(),
        caches.open('static-v1').then((cache) => cache.add('/')),
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
    console.log(version, 'fetch:', event)
  })
}

main(self as unknown as ServiceWorkerGlobalScope)

export {}
