import type * as error from '@intertwine/lib-error'
import * as random from '@intertwine/lib-random'
import * as stream from '@intertwine/lib-stream'
import * as timeBrowser from '@intertwine/lib-time/index.browser.ts'

function main(): void {
  const timeCtx = timeBrowser.initContext()

  const ctx: error.Context & stream.ServerContext = {
    ...random.initContext(),
    ...timeCtx,
    ...stream.initServerContext(timeCtx),
  }

  const entryPoints: Promise<{
    main(ctx: error.Context & stream.ServerContext): void
  }>[] = [import('@intertwine/svc-auth-guest-read/main.ts')]

  for (const entryPoint of entryPoints) {
    void (async () => {
      ;(await entryPoint).main(ctx)
    })()
  }
}

main()

export {}
