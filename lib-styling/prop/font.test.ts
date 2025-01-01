import * as styling from '@/index.ts'
import * as stylingTest from '@/test.ts'

export const url = import.meta.url

export const tests = {
  async ['variant / ligatures / basic'](
    ctx: styling.Context,
  ): Promise<void> {
    const style = [styling.font.variant.ligatures('common-ligatures')]
    const code = 'font-variant-ligatures: common-ligatures'
    await stylingTest.assertCompileBasicEquals(ctx, style, code)
  },
}
