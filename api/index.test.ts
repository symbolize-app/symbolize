import * as test from '@tiny/test/index.ts'
import * as widget from '@tiny/ui/widget.ts'
import jsdom from 'jsdom'
import * as perfHooks from 'perf_hooks'

export const all: test.TestCollection = () => []

export async function run(): Promise<boolean> {
  const dom = new jsdom.JSDOM('<!DOCTYPE html>')
  const ctx = {
    ...widget.initContext(dom.window.document),
    now: () => perfHooks.performance.now(),
  }
  const coreTest = await import('@fe/core/index.test.ts')
  const apiTest = await import('@fe/api/index.test.ts')
  const uiTest = await import('@fe/ui/index.test.ts')
  return await test.runAll(ctx, [coreTest, apiTest, uiTest])
}
