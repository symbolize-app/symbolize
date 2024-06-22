import * as contrast from '@/index.ts'
import * as contrastTest from '@/test.ts'

export const url = import.meta.url

export const tests = {
  async ['wrap / basic'](ctx: contrast.Context): Promise<void> {
    const style = [contrast.text.wrap('pretty')]
    const code = 'text-wrap: pretty'
    await contrastTest.assertCompileBasicEquals(ctx, style, code)
  },
}
