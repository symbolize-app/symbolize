import * as styling from '@/index.ts'
import * as stylingTest from '@/test.ts'

export const url = import.meta.url

export const tests = {
  async ['content / basic'](ctx: styling.Context): Promise<void> {
    const style = [styling.content(styling.stringLiteral('"\''))]
    const code = 'content: "\\"\'"'
    await stylingTest.assertCompileBasicEquals(ctx, style, code)
  },

  async ['content / concat'](ctx: styling.Context): Promise<void> {
    const style = [
      styling.content(
        styling.stringLiteral(' ('),
        styling.attr('href'),
        styling.stringLiteral(')'),
      ),
    ]
    const code = 'content: " (" attr(href) ")"'
    await stylingTest.assertCompileBasicEquals(ctx, style, code)
  },
}
