import * as contrast from '@/index.ts'
import * as contrastTest from '@/test.ts'

export const url = import.meta.url

export const tests = {
  async ['basic'](ctx: contrast.Context): Promise<void> {
    const style = [contrast.fill('context-fill')]
    const code = 'fill: context-fill'
    await contrastTest.assertCompileBasicEquals(ctx, style, code)
  },
}
