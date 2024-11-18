import * as svcReload from '@/reload.ts'
import * as compute from '@intertwine/lib-compute'
import * as contrast from '@intertwine/lib-contrast'
import type * as convey from '@intertwine/lib-convey'
import * as conveyBrowser from '@intertwine/lib-convey/index.browser.ts'
import * as random from '@intertwine/lib-random'
import * as stream from '@intertwine/lib-stream'
import * as time from '@intertwine/lib-time'

function main(): void {
  svcReload.listenForMessage()
  svcReload.listenForKeyboardShortcut()

  const worker = new Worker(
    '/.code/svc-gateway-guest-run/dedicatedWorker.ts.mjs',
    { type: 'module' },
  )

  const mainCtx: compute.Context &
    contrast.Context &
    convey.Context &
    random.Context &
    stream.WorkerClientContext &
    time.Context = {
    compute: compute.compute(),
    contrast: contrast.contrast(),
    convey: conveyBrowser.convey(),
    random: random.random(),
    streamClient: stream.workerClient(worker),
    time: time.time(),
  }

  const entryPoints = [
    import('@intertwine/svc-auth-guest-view/main.ts'),
    ...(import.meta.env.NODE_ENV === 'development' ?
      [import('@/main.development.ts')]
    : []),
  ]

  for (const entryPoint of entryPoints) {
    void (async () => {
      await (await entryPoint).main(mainCtx)
    })()
  }
}

main()
