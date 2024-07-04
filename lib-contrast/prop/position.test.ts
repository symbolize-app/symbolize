import * as contrast from '@/index.ts'
import * as contrastTest from '@/test.ts'

export const url = import.meta.url

export const tests = {
  async ['basic'](ctx: contrast.Context): Promise<void> {
    const style = [contrast.position('fixed')]
    const code = 'position: fixed'
    await contrastTest.assertCompileBasicEquals(ctx, style, code)
  },
}
