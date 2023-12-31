import * as svcReload from '@/reload.ts'
import type * as error from '@intertwine/lib-error'
import * as random from '@intertwine/lib-random'
import * as stream from '@intertwine/lib-stream'
import * as timeBrowser from '@intertwine/lib-time/index.browser.ts'
import * as widget from '@intertwine/lib-widget'

function main() {
  svcReload.listenForMessage()
  svcReload.listenForKeyboardShortcut()

  const worker = new Worker(
    '/.code/svc-gateway-guest-run/dedicatedWorker.ts.mjs',
    { type: 'module' }
  )

  const ctx: widget.Context & error.Context & stream.ClientContext = {
    ...random.initContext(),
    ...timeBrowser.initContext(),
    ...widget.initContext(window.document),
    ...stream.initClientContext(worker),
  }

  const entryPoints: Promise<{
    main(ctx: widget.Context & error.Context & stream.ClientContext): void
  }>[] = [
    import('@intertwine/svc-auth-guest-view/main.ts'),
    ...(import.meta.env.NODE_ENV === 'development'
      ? [import('@/main.development.ts')]
      : []),
  ]

  for (const entryPoint of entryPoints) {
    void (async () => {
      ;(await entryPoint).main(ctx)
    })()
  }
}

main()
