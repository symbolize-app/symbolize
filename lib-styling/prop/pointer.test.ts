import * as styling from '@/index.ts'
import * as stylingTest from '@/test.ts'

export const url = import.meta.url

export const tests = {
  async ['events / basic'](ctx: styling.Context): Promise<void> {
    const style = [styling.pointer.events('none')]
    const code = 'pointer-events: none'
    await stylingTest.assertCompileBasicEquals(ctx, style, code)
  },
}
