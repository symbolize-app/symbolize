import * as contrast from '@/index.ts'
import * as contrastTest from '@/test.ts'

export const url = import.meta.url

export const tests = {
  async ['events / basic'](ctx: contrast.Context): Promise<void> {
    const style = [contrast.pointer.events('none')]
    const code = 'pointer-events: none'
    await contrastTest.assertCompileBasicEquals(ctx, style, code)
  },
}
