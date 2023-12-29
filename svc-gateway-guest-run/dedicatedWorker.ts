import type * as error from '@intertwine/lib-error'
import * as random from '@intertwine/lib-random'
import * as timeBrowser from '@intertwine/lib-time/index.browser.ts'

declare const self: DedicatedWorkerGlobalScope

function main(): void {
  console.log('init')
  self.addEventListener('message', (event) => {
    console.log('message', event)
  })
  self.postMessage('pong')

  const ctx: error.Context = {
    ...random.initContext(),
    ...timeBrowser.initContext(),
  }

  const entryPoints: Promise<{
    main(ctx: error.Context): void
  }>[] = [import('@intertwine/svc-auth-guest-read/main.ts')]

  for (const entryPoint of entryPoints) {
    void (async () => {
      ;(await entryPoint).main(ctx)
    })()
  }
}

main()

export {}
