import * as appApiTest from '@app/api/index.test.node.ts'
import type * as tinyTime from '@tiny/core/time.ts'

export function main(ctx: tinyTime.Context): void {
  if (process.send) {
    process.send('ready')
  }

  void appApiTest.run(ctx)
}
