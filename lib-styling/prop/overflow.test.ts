import * as styling from '@/index.ts'
import * as stylingTest from '@/test.ts'
import * as test from '@symbolize/lib-test'

export const url = import.meta.url

export const tests = {
  async ['x / basic'](ctx: styling.Context): Promise<void> {
    const style = [styling.overflow.x('scroll')]
    const code = 'overflow-x: scroll'
    await stylingTest.assertCompileBasicEquals(ctx, style, code)
  },

  async ['xy / basic'](ctx: styling.Context): Promise<void> {
    const style = [styling.overflow.xy('scroll')]
    const result = await stylingTest.testCompile(ctx, style)

    test.assertDeepEquals(result.classNames, ['a0', 'a1'])
    test.assertDeepEquals(result.code, [
      stylingTest.dedent(`
        .a0 {
          overflow-x: scroll;
        }
      `),
      stylingTest.dedent(`
        .a1 {
          overflow-y: scroll;
        }
      `),
    ])
  },
}
