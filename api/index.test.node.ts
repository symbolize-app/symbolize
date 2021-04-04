import * as test from '@tiny/test/index.ts'
import * as widget from '@tiny/ui/widget.ts'
import type * as time from '@tiny/util/time.ts'
import jsdom from 'jsdom'

export const all: test.TestCollection = () => [
  import('@fe/api/payload.test.ts'),
  import('@fe/api/query.test.ts'),
]

export async function run(
  baseContext: time.Context
): Promise<boolean> {
  const dom = new jsdom.JSDOM('<!DOCTYPE html>')
  const window = dom.window
  const document = window.document
  const ctx = {
    ...baseContext,
    ...widget.initContext(document),
  }
  return await test.runAll(ctx, [
    import('@fe/core/index.test.ts'),
    import('@fe/api/index.test.node.ts'),
    import('@fe/ui/index.test.node.ts'),
    import('@tiny/api/index.test.ts'),
    import('@tiny/util/index.test.ts'),
  ])
}
