import * as styling from '@/index.ts'
import * as stylingTest from '@/test.ts'

export const url = import.meta.url

export const tests = {
  async ['basic'](ctx: styling.Context): Promise<void> {
    const style = [styling.isolation('isolate')]
    const code = 'isolation: isolate'
    await stylingTest.assertCompileBasicEquals(ctx, style, code)
  },
}
