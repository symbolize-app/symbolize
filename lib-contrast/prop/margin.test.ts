import * as contrast from '@/index.ts'
import * as contrastTest from '@/test.ts'
import * as test from '@symbolize/lib-test'

export const url = import.meta.url

export const tests = {
  async ['l / basic'](ctx: contrast.Context): Promise<void> {
    const style = [contrast.margin.l(contrast.px(1))]
    const code = 'margin-left: 1px'
    await contrastTest.assertCompileBasicEquals(ctx, style, code)
  },

  async ['oi / basic'](ctx: contrast.Context): Promise<void> {
    const style = [contrast.margin.oi(contrast.px(1))]
    const result = await contrastTest.testCompile(ctx, style)

    test.assertDeepEquals(result.classNames, ['a0', 'a1', 'a2', 'a3'])
    test.assertDeepEquals(result.code, [
      contrastTest.dedent(`
        .a0 {
          margin-block-start: 1px;
        }
      `),
      contrastTest.dedent(`
        .a1 {
          margin-block-end: 1px;
        }
      `),
      contrastTest.dedent(`
        .a2 {
          margin-inline-start: 1px;
        }
      `),
      contrastTest.dedent(`
        .a3 {
          margin-inline-end: 1px;
        }
      `),
    ])
  },
}
