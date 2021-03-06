import * as test from '@tiny/test/index.ts'

export const all: test.TestCollection = () => [
  import('@fe/ui/button.t.ts'),
]

export async function run(): Promise<boolean> {
  const ctx = {
    now: () => window.performance.now(),
  }
  const coreTest = await import('@fe/core/index.t.ts')
  const uiTest = await import('@fe/ui/index.t.ts')
  return await test.runAll(ctx, [coreTest, uiTest])
}
