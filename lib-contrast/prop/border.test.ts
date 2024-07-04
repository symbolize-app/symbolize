import * as contrast from '@/index.ts'
import * as contrastTest from '@/test.ts'
import * as test from '@intertwine/lib-test'

export const url = import.meta.url

export const tests = {
  async ['l / basic'](ctx: contrast.Context): Promise<void> {
    const style = [contrast.border.style.l('dashed')]
    const code = 'border-left-style: dashed'
    await contrastTest.assertCompileBasicEquals(ctx, style, code)
  },

  async ['oi / basic'](ctx: contrast.Context): Promise<void> {
    const style = [contrast.border.style.oi('none')]
    const result = await contrastTest.testCompile(ctx, style)

    test.assertDeepEquals(result.classNames, ['a0', 'a1', 'a2', 'a3'])
    test.assertDeepEquals(result.code, [
      contrastTest.dedent(`
        .a0 {
          border-block-start-style: none;
        }
      `),
      contrastTest.dedent(`
        .a1 {
          border-block-end-style: none;
        }
      `),
      contrastTest.dedent(`
        .a2 {
          border-inline-start-style: none;
        }
      `),
      contrastTest.dedent(`
        .a3 {
          border-inline-end-style: none;
        }
      `),
    ])
  },
}
