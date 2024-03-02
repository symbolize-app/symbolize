import * as svcReload from '@/reload.ts'
import * as compute from '@intertwine/lib-compute'
import type * as convey from '@intertwine/lib-convey'
import * as conveyBrowser from '@intertwine/lib-convey/index.browser.ts'
import * as random from '@intertwine/lib-random'
import * as stream from '@intertwine/lib-stream'
import type * as time from '@intertwine/lib-time'
import * as timeBrowser from '@intertwine/lib-time/index.browser.ts'

function main(): void {
  svcReload.listenForMessage()
  svcReload.listenForKeyboardShortcut()

  const worker = new Worker(
    '/.code/svc-gateway-guest-run/dedicatedWorker.ts.mjs',
    { type: 'module' },
  )

  const mainCtx: compute.Context &
    convey.Context &
    random.Context &
    stream.ClientContext &
    time.Context = {
    compute: new compute.Compute(),
    convey: new conveyBrowser.ConveyImpl(),
    random: new random.RandomImpl(),
    streamClient: stream.Client.init(worker),
    time: new timeBrowser.TimeImpl(),
  }

  const entryPoints: Promise<{
    main(ctx: typeof mainCtx): void
  }>[] = [
    import('@intertwine/svc-auth-guest-view/main.ts'),
    ...(import.meta.env.NODE_ENV === 'development' ?
      [import('@/main.development.ts')]
    : []),
  ]

  for (const entryPoint of entryPoints) {
    void (async () => {
      ;(await entryPoint).main(mainCtx)
    })()
  }
}

main()
