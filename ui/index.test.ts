import * as test from '@tiny/test/index.ts'

export const all: test.TestCollection = () => [
  import('@fe/ui/button.test.ts'),
]

export async function run(): Promise<boolean> {
  const ctx = {
    now: () => window.performance.now(),
  }
  const coreTest = await import('@fe/core/index.test.ts')
  const uiTest = await import('@fe/ui/index.test.ts')
  return await test.runAll(ctx, [coreTest, uiTest])
}
