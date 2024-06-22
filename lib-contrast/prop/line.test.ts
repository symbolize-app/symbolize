import * as contrast from '@/index.ts'
import * as contrastTest from '@/test.ts'

export const url = import.meta.url

export const tests = {
  async ['height / basic'](ctx: contrast.Context): Promise<void> {
    const style = [contrast.line.height(contrast.rlh(2))]
    const code = 'line-height: 2rlh'
    await contrastTest.assertCompileBasicEquals(ctx, style, code)
  },
}
