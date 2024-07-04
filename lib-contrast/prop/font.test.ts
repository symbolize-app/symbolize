import * as contrast from '@/index.ts'
import * as contrastTest from '@/test.ts'

export const url = import.meta.url

export const tests = {
  async ['variant / ligatures / basic'](
    ctx: contrast.Context,
  ): Promise<void> {
    const style = [contrast.font.variant.ligatures('common-ligatures')]
    const code = 'font-variant-ligatures: common-ligatures'
    await contrastTest.assertCompileBasicEquals(ctx, style, code)
  },
}
