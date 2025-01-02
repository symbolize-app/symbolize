import * as styling from '@/index.ts'
import * as stylingTest from '@/test.ts'

export const url = import.meta.url

export const tests = {
  async ['mode / basic'](ctx: styling.Context): Promise<void> {
    const style = [styling.hyphen.mode('auto')]
    const code = 'hyphens: auto'
    await stylingTest.assertCompileBasicEquals(ctx, style, code)
  },
}
