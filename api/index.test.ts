import * as test from '@tiny/test/index.ts'
import * as widget from '@tiny/ui/widget.ts'
import type * as time from '@tiny/util/time.ts'
import jsdom from 'jsdom'

export const all: test.TestCollection = () => [
  import('@fe/api/query.test.ts'),
]

export async function run(
  baseContext: time.Context
): Promise<boolean> {
  const dom = new jsdom.JSDOM('<!DOCTYPE html>')
  const document = dom.window.document
  const ctx = {
    ...baseContext,
    ...widget.initContext(document),
  }
  const coreTest = await import('@fe/core/index.test.ts')
  const apiTest = await import('@fe/api/index.test.ts')
  const uiTest = await import('@fe/ui/index.test.ts')
  const tinyUtilTest = await import(
    '@tiny/util/index.test.ts'
  )
  return await test.runAll(ctx, [
    coreTest,
    apiTest,
    uiTest,
    tinyUtilTest,
  ])
}
