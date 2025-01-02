import * as styling from '@/index.ts'
import * as stylingTest from '@/test.ts'

export const url = import.meta.url

export const tests = {
  async ['wrap / basic'](ctx: styling.Context): Promise<void> {
    const style = [styling.text.wrap('pretty')]
    const code = 'text-wrap: pretty'
    await stylingTest.assertCompileBasicEquals(ctx, style, code)
  },
}
