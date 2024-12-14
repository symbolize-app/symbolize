import * as random from '@symbolize/lib-random'
import * as stream from '@symbolize/lib-stream'
import * as time from '@symbolize/lib-time'

function main(): void {
  const timeObj = time.time()

  const mainCtx = {
    random: random.random(),
    stream: stream.stream(),
    streamServer: stream.workerServer({ time: timeObj }),
    time: timeObj,
  }

  const entryPoints = [import('@symbolize/svc-auth-guest-read/main.ts')]

  for (const entryPoint of entryPoints) {
    void (async () => {
      ;(await entryPoint).main(mainCtx)
    })()
  }
}

main()

export {}
