import * as styling from '@/index.ts'
import * as stylingTest from '@/test.ts'
import * as test from '@symbolize/lib-test'

export const url = import.meta.url

export const tests = {
  async ['l / basic'](ctx: styling.Context): Promise<void> {
    const style = [styling.padding.l(styling.px(1))]
    const code = 'padding-left: 1px'
    await stylingTest.assertCompileBasicEquals(ctx, style, code)
  },

  async ['oi / basic'](ctx: styling.Context): Promise<void> {
    const style = [styling.padding.oi(styling.px(1))]
    const result = await stylingTest.testCompile(ctx, style)

    test.assertDeepEquals(result.classNames, ['a0', 'a1', 'a2', 'a3'])
    test.assertDeepEquals(result.code, [
      stylingTest.dedent(`
        .a0 {
          padding-block-start: 1px;
        }
      `),
      stylingTest.dedent(`
        .a1 {
          padding-block-end: 1px;
        }
      `),
      stylingTest.dedent(`
        .a2 {
          padding-inline-start: 1px;
        }
      `),
      stylingTest.dedent(`
        .a3 {
          padding-inline-end: 1px;
        }
      `),
    ])
  },
}
