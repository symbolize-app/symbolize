import * as contrast from '@/index.ts'
import * as contrastTest from '@/test.ts'

export const url = import.meta.url

export const tests = {
  async ['color / basic'](ctx: contrast.Context): Promise<void> {
    const style = [
      contrast.caret.color(
        contrast.rgb(contrast.pct(100), contrast.pct(0), contrast.pct(50)),
      ),
    ]
    const code = 'caret-color: rgb(100% 0% 50%)'
    await contrastTest.assertCompileBasicEquals(ctx, style, code)
  },
}
