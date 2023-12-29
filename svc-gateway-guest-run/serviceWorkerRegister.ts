import * as svcReload from '@/reload.ts'

async function main(): Promise<void> {
  svcReload.listenForMessage()

  navigator.serviceWorker.addEventListener('controllerchange', () => {
    console.log('controller change')
  })

  const reg = await navigator.serviceWorker.register(
    '/.code/svc-gateway-guest-run/serviceWorkerShell.js',
    {
      scope: '/',
      updateViaCache: 'none',
    }
  )

  reg.addEventListener('updatefound', () => {
    console.log('update found')
  })
}

void main()

export {}
