import * as contrast from '@/index.ts'
import * as contrastTest from '@/test.ts'
import * as test from '@symbolize/lib-test'

export const url = import.meta.url

export const tests = {
  async ['color / basic'](ctx: contrast.Context): Promise<void> {
    const style = [
      contrast.background.color(
        contrast.rgb(contrast.pct(100), contrast.pct(0), contrast.pct(50)),
      ),
    ]
    const code = 'background-color: rgb(100% 0% 50%)'
    await contrastTest.assertCompileBasicEquals(ctx, style, code)
  },

  async ['image / multi'](ctx: contrast.Context): Promise<void> {
    const style = [
      contrast.background.image(
        contrast.gradient.linear(
          contrast.deg(1),
          contrast.rgb(contrast.pct(0), contrast.pct(0), contrast.pct(2)),
          contrast.rgb(contrast.pct(0), contrast.pct(0), contrast.pct(3)),
        ),
        contrast.gradient.linear(
          contrast.deg(4),
          contrast.rgb(contrast.pct(0), contrast.pct(0), contrast.pct(5)),
          contrast.pct(6),
          [
            contrast.rgb(
              contrast.pct(0),
              contrast.pct(0),
              contrast.pct(7),
            ),
            contrast.pct(8),
            contrast.pct(9),
          ],
        ),
      ),
    ]
    const result = await contrastTest.testCompile(ctx, style)

    test.assertDeepEquals(result.classNames, ['a0'])
    test.assertDeepEquals(result.code, [
      contrastTest.dedent(`
        .a0 {
          background-image: linear-gradient(1deg, rgb(0% 0% 2%), rgb(0% 0% 3%)),
            linear-gradient(4deg, rgb(0% 0% 5%), 6%, rgb(0% 0% 7%) 8% 9%);
        }
      `),
    ])
  },

  async ['size / multi'](ctx: contrast.Context): Promise<void> {
    const style = [
      contrast.background.size('cover', [contrast.px(1), 'auto']),
    ]
    const result = await contrastTest.testCompile(ctx, style)

    test.assertDeepEquals(result.classNames, ['a0'])
    test.assertDeepEquals(result.code, [
      contrastTest.dedent(`
        .a0 {
          background-size:
            cover,
            1px auto;
        }
      `),
    ])
  },
}
