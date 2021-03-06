import * as test from '@tiny/test/index.ts'
import perfHooks from 'perf_hooks'
import url from 'url'

export const all: test.TestCollection = () => []

export async function run(): Promise<boolean> {
  const ctx = {
    now: () => perfHooks.performance.now(),
  }
  const coreTest = await import('@fe/core/index.t.ts')
  const apiTest = await import('@fe/api/index.t.ts')
  return await test.runAll(ctx, [coreTest, apiTest])
}

if (
  process.argv[1] === url.fileURLToPath(import.meta.url)
) {
  void run().then((success) => {
    process.exit(success ? 0 : 1)
  })
}
