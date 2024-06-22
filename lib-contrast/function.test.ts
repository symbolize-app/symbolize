import * as contrast from '@/index.ts'
import * as contrastTest from '@/test.ts'

export const url = import.meta.url

export const tests = {
  async ['add'](ctx: contrast.Context): Promise<void> {
    const style = [
      contrast.background.color(
        contrast.rgb(
          contrast.pct(0),
          contrast.pct(0),
          contrast.add(contrast.pct(1), contrast.pct(2), contrast.pct(3)),
        ),
      ),
    ]
    const code = 'background-color: rgb(0% 0% calc(1% + 2% + 3%))'
    await contrastTest.assertCompileBasicEquals(ctx, style, code)
  },

  async ['sub'](ctx: contrast.Context): Promise<void> {
    const style = [
      contrast.background.color(
        contrast.rgb(
          contrast.pct(0),
          contrast.pct(0),
          contrast.sub(contrast.pct(10), contrast.pct(2), contrast.pct(3)),
        ),
      ),
    ]
    const code = 'background-color: rgb(0% 0% calc(10% - 2% - 3%))'
    await contrastTest.assertCompileBasicEquals(ctx, style, code)
  },

  async ['concat'](ctx: contrast.Context): Promise<void> {
    const style = [
      contrast.content(
        contrast.concat(
          contrast.stringLiteral(' ('),
          contrast.attr('href'),
          contrast.stringLiteral(')'),
        ),
      ),
    ]
    const code = 'content: " (" attr(href) ")"'
    await contrastTest.assertCompileBasicEquals(ctx, style, code)
  },
}
