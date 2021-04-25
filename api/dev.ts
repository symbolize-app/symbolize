import * as apiTest from '@fe/api/index.test.node.ts'
import type * as time from '@tiny/core/time.ts'

export function main(ctx: time.Context): void {
  if (process.send) {
    process.send('ready')
  }

  void apiTest.run(ctx)
}
