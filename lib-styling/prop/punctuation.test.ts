import * as styling from '@/index.ts'
import * as stylingTest from '@/test.ts'

export const url = import.meta.url

export const tests = {
  async ['hang / basic'](ctx: styling.Context): Promise<void> {
    const style = [styling.punctuation.hang('first', 'last')]
    const code = 'hanging-punctuation: first last'
    await stylingTest.assertCompileBasicEquals(ctx, style, code)
  },
}
