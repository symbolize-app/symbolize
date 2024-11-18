import * as compute from '@intertwine/lib-compute'
import * as contrast from '@intertwine/lib-contrast'
import * as conveyTestBrowser from '@intertwine/lib-convey/test.browser.ts'
import * as randomTest from '@intertwine/lib-random/test.ts'
import * as streamTest from '@intertwine/lib-stream/test.ts'
import * as testRunner from '@intertwine/lib-test-runner'
import type * as time from '@intertwine/lib-time'
import * as timeTest from '@intertwine/lib-time/test.ts'

export async function main(ctx: time.Context): Promise<void> {
  await testRunner.runAll(ctx, [import('@intertwine/dev-pnpm-test')], {
    compute() {
      return compute.compute()
    },
    contrast() {
      return contrast.contrast()
    },
    convey(defer) {
      const convey = conveyTestBrowser.convey()
      defer(() => {
        convey.dispose()
      })
      return convey
    },
    random() {
      return randomTest.random()
    },
    stream() {
      return streamTest.stream()
    },
    time() {
      return timeTest.time()
    },
  })
}
