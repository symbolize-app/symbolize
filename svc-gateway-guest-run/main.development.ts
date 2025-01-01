import * as compute from '@symbolize/lib-compute'
import * as contrast from '@symbolize/lib-contrast'
import * as markupTestBrowser from '@symbolize/lib-markup/test.browser.ts'
import * as randomTest from '@symbolize/lib-random/test.ts'
import * as streamTest from '@symbolize/lib-stream/test.ts'
import * as testRunner from '@symbolize/lib-test-runner'
import type * as time from '@symbolize/lib-time'
import * as timeTest from '@symbolize/lib-time/test.ts'

export async function main(ctx: time.Context): Promise<void> {
  await testRunner.runAll(ctx, [import('@symbolize/dev-pnpm-test')], {
    compute() {
      return compute.compute()
    },
    contrast() {
      return contrast.contrast()
    },
    markup(defer) {
      const markup = markupTestBrowser.markup()
      defer(() => {
        markup.dispose()
      })
      return markup
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
