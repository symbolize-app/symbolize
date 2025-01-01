import * as styling from '@/index.ts'
import * as stylingTest from '@/test.ts'
import * as test from '@symbolize/lib-test'

export const url = import.meta.url

export const tests = {
  async ['l / basic'](ctx: styling.Context): Promise<void> {
    const style = [styling.inset.l(styling.px(1))]
    const code = 'left: 1px'
    await stylingTest.assertCompileBasicEquals(ctx, style, code)
  },

  async ['oi / basic'](ctx: styling.Context): Promise<void> {
    const style = [styling.inset.oi(styling.px(1))]
    const result = await stylingTest.testCompile(ctx, style)

    test.assertDeepEquals(result.classNames, ['a0', 'a1', 'a2', 'a3'])
    test.assertDeepEquals(result.code, [
      stylingTest.dedent(`
        .a0 {
          inset-block-start: 1px;
        }
      `),
      stylingTest.dedent(`
        .a1 {
          inset-block-end: 1px;
        }
      `),
      stylingTest.dedent(`
        .a2 {
          inset-inline-start: 1px;
        }
      `),
      stylingTest.dedent(`
        .a3 {
          inset-inline-end: 1px;
        }
      `),
    ])
  },
}
