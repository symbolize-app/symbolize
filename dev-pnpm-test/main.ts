#!/usr/bin/env node-loader
import * as test from '@intertwine/lib-test'
import type * as time from '@intertwine/lib-time'
import * as timeNode from '@intertwine/lib-time/index.node.ts'
import * as nodeFs from 'node:fs'
import * as nodeUrl from 'node:url'

export const all: test.TestCollection = () => []

export async function run(baseContext: time.Context): Promise<boolean> {
  const ctx = {
    ...baseContext,
  }
  return await test.runAll(ctx, [import('@/index.ts')])
}

if (
  process.argv[1] &&
  nodeFs.realpathSync(process.argv[1]) ===
    nodeUrl.fileURLToPath(import.meta.url)
) {
  void run({
    ...timeNode.initContext(),
  }).then((success) => {
    process.exit(success ? 0 : 1)
  })
}
