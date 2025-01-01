import * as styling from '@/index.ts'
import * as stylingTest from '@/test.ts'
import * as test from '@symbolize/lib-test'

export const url = import.meta.url

export const tests = {
  async ['l / basic'](ctx: styling.Context): Promise<void> {
    const style = [styling.border.style.l('dashed')]
    const code = 'border-left-style: dashed'
    await stylingTest.assertCompileBasicEquals(ctx, style, code)
  },

  async ['oi / basic'](ctx: styling.Context): Promise<void> {
    const style = [styling.border.style.oi('none')]
    const result = await stylingTest.testCompile(ctx, style)

    test.assertDeepEquals(result.classNames, ['a0', 'a1', 'a2', 'a3'])
    test.assertDeepEquals(result.code, [
      stylingTest.dedent(`
        .a0 {
          border-block-start-style: none;
        }
      `),
      stylingTest.dedent(`
        .a1 {
          border-block-end-style: none;
        }
      `),
      stylingTest.dedent(`
        .a2 {
          border-inline-start-style: none;
        }
      `),
      stylingTest.dedent(`
        .a3 {
          border-inline-end-style: none;
        }
      `),
    ])
  },
}
