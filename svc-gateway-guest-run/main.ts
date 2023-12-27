import type * as error from '@intertwine/lib-error'
import * as random from '@intertwine/lib-random'
import * as timeBrowser from '@intertwine/lib-time/index.browser.ts'
import * as widget from '@intertwine/lib-widget'

function main() {
  const ctx: widget.Context & error.Context = {
    ...random.initContext(),
    ...timeBrowser.initContext(),
    ...widget.initContext(window.document),
  }

  const entryPoints: Promise<{
    main(ctx: widget.Context & error.Context): void
  }>[] = [
    import('@intertwine/svc-auth-guest-view/main.ts'),
    ...(import.meta.env.NODE_ENV === 'development'
      ? [import('@/main.development.ts')]
      : []),
  ]

  for (const entryPoint of entryPoints) {
    ;(async () => {
      ;(await entryPoint).main(ctx)
    })().catch(console.error)
  }
}

main()
