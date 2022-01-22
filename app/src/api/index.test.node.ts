import type * as tinyTime from '@tiny/core/time.ts'
import * as tinyTest from '@tiny/test/index.ts'
import * as tinyWidget from '@tiny/ui/widget.ts'
import jsdom from 'jsdom'

export const all: tinyTest.TestCollection = () => [
  import('@fe/api/route/member.test.ts'),
  import('@fe/api/route/search.test.ts'),
]

export async function run(
  baseContext: tinyTime.Context
): Promise<boolean> {
  const dom = new jsdom.JSDOM('<!DOCTYPE html>')
  const window = dom.window
  const document = window.document
  const ctx = {
    ...baseContext,
    ...tinyWidget.initContext(document),
  }
  return await tinyTest.runAll(ctx, [
    import('@fe/core/index.test.ts'),
    import('@fe/api/index.test.node.ts'),
    import('@fe/ui/index.test.node.ts'),
    import('@tiny/core/index.test.ts'),
  ])
}
