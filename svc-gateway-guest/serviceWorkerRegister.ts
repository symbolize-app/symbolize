async function main(): Promise<void> {
  const id = Math.random()

  navigator.serviceWorker.addEventListener(
    'message',
    (event) => {
      console.log(id, 'message:', event)

      navigator.serviceWorker.controller?.postMessage([
        'TEST1',
        id,
      ])
    }
  )

  navigator.serviceWorker.addEventListener(
    'controllerchange',
    () => {
      console.log(id, 'controller change')

      navigator.serviceWorker.controller?.postMessage([
        'TEST2',
        id,
      ])
    }
  )

  const reg = await navigator.serviceWorker.register(
    '/js/svc-gateway-guest/serviceWorker.ts.js',
    {
      scope: '/',
      updateViaCache: 'none',
    }
  )
  reg.addEventListener('updatefound', () => {
    console.log(id, 'update found')
  })
}

main().catch(console.error)

export {}
