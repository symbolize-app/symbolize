import type * as test from '@tiny/test/index.ts'

export const all: test.TestCollection = () => [
  import('@fe/ui/button.t.ts'),
]

export async function run(): Promise<void> {
  const test = await import('@tiny/test/index.ts')
  const coreTest = await import('@fe/core/index.t.ts')
  const uiTest = await import('@fe/ui/index.t.ts')
  await test.runAll([coreTest, uiTest])
}
