import * as svcReload from '@/reload.ts'
import * as compute from '@symbolize/lib-compute'
import * as contrast from '@symbolize/lib-contrast'
import type * as markup from '@symbolize/lib-markup'
import * as markupBrowser from '@symbolize/lib-markup/index.browser.ts'
import * as random from '@symbolize/lib-random'
import * as stream from '@symbolize/lib-stream'
import * as time from '@symbolize/lib-time'

function main(): void {
  svcReload.listenForMessage()
  svcReload.listenForKeyboardShortcut()

  const worker = new Worker(
    '/.code/svc-gateway-guest-run/dedicatedWorker.ts.mjs',
    { type: 'module' },
  )

  const mainCtx: compute.Context &
    contrast.Context &
    markup.Context &
    random.Context &
    stream.WorkerClientContext &
    time.Context = {
    compute: compute.compute(),
    contrast: contrast.contrast(),
    markup: markupBrowser.markup(),
    random: random.random(),
    streamClient: stream.workerClient(worker),
    time: time.time(),
  }

  const entryPoints = [
    import('@symbolize/svc-auth-guest-view/main.ts'),
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
