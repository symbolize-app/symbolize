import * as contrast from '@/index.ts'
import * as contrastTest from '@/test.ts'

export const url = import.meta.url

export const tests = {
  async ['mode / basic'](ctx: contrast.Context): Promise<void> {
    const style = [contrast.size.mode('border-box')]
    const code = 'box-sizing: border-box'
    await contrastTest.assertCompileBasicEquals(ctx, style, code)
  },

  async ['min / h / basic'](ctx: contrast.Context): Promise<void> {
    const style = [contrast.size.min.h(contrast.px(10))]
    const code = 'min-height: 10px'
    await contrastTest.assertCompileBasicEquals(ctx, style, code)
  },

  async ['max / h / basic'](ctx: contrast.Context): Promise<void> {
    const style = [contrast.size.max.h(contrast.px(10))]
    const code = 'max-height: 10px'
    await contrastTest.assertCompileBasicEquals(ctx, style, code)
  },

  async ['h / basic'](ctx: contrast.Context): Promise<void> {
    const style = [contrast.size.h(contrast.px(10))]
    const code = 'height: 10px'
    await contrastTest.assertCompileBasicEquals(ctx, style, code)
  },
}
