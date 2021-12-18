import type * as time from '@tiny/core/time.ts'
import * as test from '@tiny/test/index.ts'
import * as widget from '@tiny/ui/widget.ts'
import jsdom from 'jsdom'

export const all: test.TestCollection = () => [
  import('@fe/api/route/member.test.ts'),
  import('@fe/api/route/search.test.ts'),
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
    import('@tiny/core/index.test.ts'),
  ])
}
