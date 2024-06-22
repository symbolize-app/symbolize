import * as contrast from '@/index.ts'
import * as contrastTest from '@/test.ts'

export const url = import.meta.url

export const tests = {
  async ['content'](ctx: contrast.Context): Promise<void> {
    const style = [contrast.content(contrast.stringLiteral('"\''))]
    const code = 'content: "\\"\'"'
    await contrastTest.assertCompileBasicEquals(ctx, style, code)
  },
}
