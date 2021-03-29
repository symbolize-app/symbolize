import * as test from '@tiny/test/index.ts'
import * as widget from '@tiny/ui/widget.ts'

export const all: test.TestCollection<widget.WidgetContext> = () => [
  import('@fe/ui/button.test.ts'),
]

export async function run(): Promise<boolean> {
  const document = window.document.implementation.createHTMLDocument()
  const ctx: widget.WidgetContext & test.TestRunContext = {
    ...widget.initContext(document),
    performanceNow: () => window.performance.now(),
    setTimeout: (...args) => window.setTimeout(...args),
  }
  const coreTest = await import('@fe/core/index.test.ts')
  const uiTest = await import('@fe/ui/index.test.ts')
  const tinyUtilTest = await import(
    '@tiny/util/index.test.ts'
  )
  return await test.runAll(ctx, [
    coreTest,
    uiTest,
    tinyUtilTest,
  ])
}
