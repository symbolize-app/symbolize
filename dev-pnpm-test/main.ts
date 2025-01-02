#!/usr/bin/env node-loader
import * as compute from '@symbolize/lib-compute'
import * as markupNode from '@symbolize/lib-markup/index.node.ts'
import * as randomTest from '@symbolize/lib-random/test.ts'
import * as streamTest from '@symbolize/lib-stream/test.ts'
import * as styling from '@symbolize/lib-styling'
import * as testRunner from '@symbolize/lib-test-runner'
import * as time from '@symbolize/lib-time'
import * as timeTest from '@symbolize/lib-time/test.ts'
import * as nodeFs from 'node:fs'
import * as nodeUrl from 'node:url'

export async function run(baseContext: time.Context): Promise<boolean> {
  const ctx = {
    ...baseContext,
  }
  return testRunner.runAll(ctx, [import('@/index.ts')], {
    compute() {
      return compute.compute()
    },
    markup(defer) {
      const markup = markupNode.markup()
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

if (
  process.argv[1] &&
  nodeFs.realpathSync(process.argv[1]) ===
    nodeUrl.fileURLToPath(import.meta.url)
) {
  void run({
    time: time.time(),
  }).then((success) => {
    process.exit(success ? 0 : 1)
  })
}
