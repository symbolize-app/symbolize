import * as contrast from '@/index.ts'
import * as contrastTest from '@/test.ts'

export const url = import.meta.url

export const tests = {
  async ['mode / basic'](ctx: contrast.Context): Promise<void> {
    const style = [contrast.hyphen.mode('auto')]
    const code = 'hyphens: auto'
    await contrastTest.assertCompileBasicEquals(ctx, style, code)
  },
}
