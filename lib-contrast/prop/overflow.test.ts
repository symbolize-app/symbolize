import * as contrast from '@/index.ts'
import * as contrastTest from '@/test.ts'
import * as test from '@intertwine/lib-test'

export const url = import.meta.url

export const tests = {
  async ['x / basic'](ctx: contrast.Context): Promise<void> {
    const style = [contrast.overflow.x('scroll')]
    const code = 'overflow-x: scroll'
    await contrastTest.assertCompileBasicEquals(ctx, style, code)
  },

  async ['xy / basic'](ctx: contrast.Context): Promise<void> {
    const style = [contrast.overflow.xy('scroll')]
    const result = await contrastTest.testCompile(ctx, style)

    test.assertDeepEquals(result.classNames, ['a0', 'a1'])
    test.assertDeepEquals(result.code, [
      contrastTest.dedent(`
        .a0 {
          overflow-x: scroll;
        }
      `),
      contrastTest.dedent(`
        .a1 {
          overflow-y: scroll;
        }
      `),
    ])
  },
}
