import * as styling from '@/index.ts'
import * as stylingTest from '@/test.ts'

export const url = import.meta.url

export const tests = {
  async ['color / basic'](ctx: styling.Context): Promise<void> {
    const style = [
      styling.caret.color(
        styling.rgb(styling.pct(100), styling.pct(0), styling.pct(50)),
      ),
    ]
    const code = 'caret-color: rgb(100% 0% 50%)'
    await stylingTest.assertCompileBasicEquals(ctx, style, code)
  },
}
