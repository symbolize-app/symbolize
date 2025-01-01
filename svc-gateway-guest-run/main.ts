import * as svcReload from '@/reload.ts'
import * as compute from '@symbolize/lib-compute'
import type * as markup from '@symbolize/lib-markup'
import * as markupBrowser from '@symbolize/lib-markup/index.browser.ts'
import * as random from '@symbolize/lib-random'
import * as stream from '@symbolize/lib-stream'
import * as styling from '@symbolize/lib-styling'
import * as time from '@symbolize/lib-time'

function main(): void {
  svcReload.listenForMessage()
  svcReload.listenForKeyboardShortcut()

  const worker = new Worker(
    '/.code/svc-gateway-guest-run/dedicatedWorker.ts.mjs',
    { type: 'module' },
  )

  const mainCtx: compute.Context &
    markup.Context &
    random.Context &
    stream.WorkerClientContext &
    styling.Context &
    time.Context = {
    compute: compute.compute(),
    markup: markupBrowser.markup(),
    random: random.random(),
    streamClient: stream.workerClient(worker),
    styling: styling.styling(),
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
