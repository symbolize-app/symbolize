import * as contrast from '@/index.ts'
import * as contrastTest from '@/test.ts'

export const url = import.meta.url

export const tests = {
  async ['basic'](ctx: contrast.Context): Promise<void> {
    const style = [contrast.isolation('isolate')]
    const code = 'isolation: isolate'
    await contrastTest.assertCompileBasicEquals(ctx, style, code)
  },
}
