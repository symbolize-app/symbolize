import * as test from '@tiny/test/index.ts'
import jsdom from 'jsdom'
import url from 'url'
import vm from 'vm'

export const all: test.TestCollection = () => []

export async function run(): Promise<void> {
  const dom = new jsdom.JSDOM('', {
    runScripts: 'outside-only',
  })
  const script = new vm.Script(`
    if (!this.ran) {
      this.ran = 0;
    }

    ++this.ran;
  `)

  const vmContext = dom.getInternalVMContext()

  script.runInContext(vmContext)
  script.runInContext(vmContext)
  script.runInContext(vmContext)

  console.assert(dom.window.ran === 4)

  const coreTest = await import('@fe/core/index.t.ts')
  const apiTest = await import('@fe/api/index.t.ts')
  const uiTest = await import('@fe/ui/index.t.ts')
  await test.runAll([coreTest, apiTest, uiTest])
}

if (
  process.argv[1] === url.fileURLToPath(import.meta.url)
) {
  run().catch((error) => {
    throw error
  })
}
