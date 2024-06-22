import * as contrast from '@/index.ts'
import * as contrastTest from '@/test.ts'
import * as test from '@intertwine/lib-test'

export const url = import.meta.url

export const tests = {
  async ['l / basic'](ctx: contrast.Context): Promise<void> {
    const style = [contrast.padding.l(contrast.px(1))]
    const code = 'padding-left: 1px'
    await contrastTest.assertCompileBasicEquals(ctx, style, code)
  },

  async ['oi / basic'](ctx: contrast.Context): Promise<void> {
    const style = [contrast.padding.oi(contrast.px(1))]
    const result = await contrastTest.testCompile(ctx, style)

    test.assertDeepEquals(result.classNames, ['a0', 'a1', 'a2', 'a3'])
    test.assertDeepEquals(result.code, [
      contrastTest.dedent(`
        .a0 {
          padding-block-start: 1px;
        }
      `),
      contrastTest.dedent(`
        .a1 {
          padding-block-end: 1px;
        }
      `),
      contrastTest.dedent(`
        .a2 {
          padding-inline-start: 1px;
        }
      `),
      contrastTest.dedent(`
        .a3 {
          padding-inline-end: 1px;
        }
      `),
    ])
  },
}
