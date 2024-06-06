import * as contrast from '@/index.ts'
import * as contrastTest from '@/test.ts'
import * as test from '@intertwine/lib-test'

export const url = import.meta.url

export const tests = {
  async ['variant / ligatures / basic'](
    ctx: contrast.Context,
  ): Promise<void> {
    const style = [contrast.font.variant.ligatures('common-ligatures')]
    const result = await contrastTest.testCompile(ctx, style)

    test.assertDeepEquals(result.classNames, ['a0'])
    test.assertDeepEquals(result.code, [
      contrastTest.dedent(`
        .a0 {
          font-variant-ligatures: common-ligatures;
        }
      `),
    ])
  },
}
