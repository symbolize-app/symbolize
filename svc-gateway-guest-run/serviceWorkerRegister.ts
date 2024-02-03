import * as svcReload from '@/reload.ts'

async function main(): Promise<void> {
  svcReload.listenForMessage()

  navigator.serviceWorker.addEventListener('controllerchange', () => {
    // eslint-disable-next-line no-console
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
    // eslint-disable-next-line no-console
    console.log('update found')
  })
}

void main()

export {}
