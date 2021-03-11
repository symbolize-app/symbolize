import * as test from '@tiny/test/index.ts'
import * as perfHooks from 'perf_hooks'

export const all: test.TestCollection = () => []

export async function run(): Promise<boolean> {
  const ctx = {
    now: () => perfHooks.performance.now(),
  }
  const coreTest = await import('@fe/core/index.test.ts')
  const apiTest = await import('@fe/api/index.test.ts')
  return await test.runAll(ctx, [coreTest, apiTest])
}
