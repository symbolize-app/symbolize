import * as test from '@tiny/test/index.ts'
import * as widget from '@tiny/ui/widget.ts'

export const all: test.TestCollection<
  widget.WidgetContext & test.TestContext
> = () => [import('@fe/ui/button.test.ts')]

export async function run(): Promise<boolean> {
  const document = window.document.implementation.createHTMLDocument()
  const ctx = {
    ...widget.initContext(document),
    now: () => window.performance.now(),
  }
  const coreTest = await import('@fe/core/index.test.ts')
  const uiTest = await import('@fe/ui/index.test.ts')
  return await test.runAll(ctx, [coreTest, uiTest])
}
