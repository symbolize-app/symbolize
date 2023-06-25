import * as tinyTest from '@intertwine/test/index.ts'
import * as tinyTimeNode from '@intertwine/time/time.node.ts'
import type * as tinyTime from '@intertwine/time/time.ts'
import * as tinyWidget from '@intertwine/widget/widget.ts'
import jsdom from 'jsdom'
import * as nodeUrl from 'node:url'

export const all: tinyTest.TestCollection = () => []

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
    import('@/index.test.browser.ts'),
  ])
}

if (
  process.argv[1] === nodeUrl.fileURLToPath(import.meta.url)
) {
  void run({
    ...tinyTimeNode.initContext(),
  }).then((success) => {
    process.exit(success ? 0 : 1)
  })
}
