import * as compute from '@symbolize/lib-compute'
import * as markupTestBrowser from '@symbolize/lib-markup/test.browser.ts'
import * as randomTest from '@symbolize/lib-random/test.ts'
import * as streamTest from '@symbolize/lib-stream/test.ts'
import * as styling from '@symbolize/lib-styling'
import * as testRunner from '@symbolize/lib-test-runner'
import type * as time from '@symbolize/lib-time'
import * as timeTest from '@symbolize/lib-time/test.ts'

export async function main(ctx: time.Context): Promise<void> {
  await testRunner.runAll(ctx, [import('@symbolize/dev-pnpm-test')], {
    compute() {
      return compute.compute()
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
    styling() {
      return styling.styling()
    },
    time() {
      return timeTest.time()
    },
  })
}
