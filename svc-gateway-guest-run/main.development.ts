import * as compute from '@intertwine/lib-compute'
import * as conveyTestBrowser from '@intertwine/lib-convey/test.browser.ts'
import * as randomTest from '@intertwine/lib-random/test.ts'
import * as testRunner from '@intertwine/lib-test-runner'
import type * as time from '@intertwine/lib-time'
import * as timeTest from '@intertwine/lib-time/test.ts'

export async function main(ctx: time.Context): Promise<void> {
  await testRunner.runAll(
    ctx,
    [import('@intertwine/dev-pnpm-test')],
    (defer) => {
      const convey = new conveyTestBrowser.ConveyImpl()
      defer(() => {
        convey.dispose()
      })
      return {
        compute: new compute.Compute(),
        convey,
        random: randomTest.RandomImpl.build(),
        time: timeTest.TimeImpl.build(),
      }
    },
  )
}
