import type * as test from '@tiny/test/index.ts'
import url from 'url'

export const all: test.TestCollection = () => []

export async function run(): Promise<void> {
  const test = await import('@tiny/test/index.ts')
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
