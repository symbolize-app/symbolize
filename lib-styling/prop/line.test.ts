import * as styling from '@/index.ts'
import * as stylingTest from '@/test.ts'

export const url = import.meta.url

export const tests = {
  async ['height / basic'](ctx: styling.Context): Promise<void> {
    const style = [styling.line.height(styling.rlh(2))]
    const code = 'line-height: 2rlh'
    await stylingTest.assertCompileBasicEquals(ctx, style, code)
  },
}
