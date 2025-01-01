import * as styling from '@/index.ts'
import * as stylingTest from '@/test.ts'

export const url = import.meta.url

export const tests = {
  async ['mode / basic'](ctx: styling.Context): Promise<void> {
    const style = [styling.size.mode('border-box')]
    const code = 'box-sizing: border-box'
    await stylingTest.assertCompileBasicEquals(ctx, style, code)
  },

  async ['min / h / basic'](ctx: styling.Context): Promise<void> {
    const style = [styling.size.min.h(styling.px(10))]
    const code = 'min-height: 10px'
    await stylingTest.assertCompileBasicEquals(ctx, style, code)
  },

  async ['max / h / basic'](ctx: styling.Context): Promise<void> {
    const style = [styling.size.max.h(styling.px(10))]
    const code = 'max-height: 10px'
    await stylingTest.assertCompileBasicEquals(ctx, style, code)
  },

  async ['h / basic'](ctx: styling.Context): Promise<void> {
    const style = [styling.size.h(styling.px(10))]
    const code = 'height: 10px'
    await stylingTest.assertCompileBasicEquals(ctx, style, code)
  },
}
