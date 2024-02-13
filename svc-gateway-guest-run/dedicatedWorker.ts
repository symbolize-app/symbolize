import type * as error from '@intertwine/lib-error'
import * as random from '@intertwine/lib-random'
import * as stream from '@intertwine/lib-stream'
import * as timeBrowser from '@intertwine/lib-time/index.browser.ts'

function main(): void {
  const timeObj = new timeBrowser.TimeImpl()

  const ctx: error.Context & stream.ServerContext = {
    random: new random.RandomImpl(),
    streamServer: stream.Server.init({ time: timeObj }),
    time: timeObj,
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
