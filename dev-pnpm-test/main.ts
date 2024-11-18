#!/usr/bin/env node-loader
import * as compute from '@intertwine/lib-compute'
import * as contrast from '@intertwine/lib-contrast'
import * as conveyNode from '@intertwine/lib-convey/index.node.ts'
import * as randomTest from '@intertwine/lib-random/test.ts'
import * as streamTest from '@intertwine/lib-stream/test.ts'
import * as testRunner from '@intertwine/lib-test-runner'
import * as time from '@intertwine/lib-time'
import * as timeTest from '@intertwine/lib-time/test.ts'
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
    contrast() {
      return contrast.contrast()
    },
    convey(defer) {
      const convey = conveyNode.convey()
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
