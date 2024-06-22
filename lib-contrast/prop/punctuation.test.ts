import * as contrast from '@/index.ts'
import * as contrastTest from '@/test.ts'

export const url = import.meta.url

export const tests = {
  async ['hang / basic'](ctx: contrast.Context): Promise<void> {
    const style = [contrast.punctuation.hang('first', 'last')]
    const code = 'hanging-punctuation: first last'
    await contrastTest.assertCompileBasicEquals(ctx, style, code)
  },
}
