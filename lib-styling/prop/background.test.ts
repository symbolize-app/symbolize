import * as styling from '@/index.ts'
import * as stylingTest from '@/test.ts'
import * as test from '@symbolize/lib-test'

export const url = import.meta.url

export const tests = {
  async ['color / basic'](ctx: styling.Context): Promise<void> {
    const style = [
      styling.background.color(
        styling.rgb(styling.pct(100), styling.pct(0), styling.pct(50)),
      ),
    ]
    const code = 'background-color: rgb(100% 0% 50%)'
    await stylingTest.assertCompileBasicEquals(ctx, style, code)
  },

  async ['image / multi'](ctx: styling.Context): Promise<void> {
    const style = [
      styling.background.image(
        styling.gradient.linear(
          styling.deg(1),
          styling.rgb(styling.pct(0), styling.pct(0), styling.pct(2)),
          styling.rgb(styling.pct(0), styling.pct(0), styling.pct(3)),
        ),
        styling.gradient.linear(
          styling.deg(4),
          styling.rgb(styling.pct(0), styling.pct(0), styling.pct(5)),
          styling.pct(6),
          [
            styling.rgb(styling.pct(0), styling.pct(0), styling.pct(7)),
            styling.pct(8),
            styling.pct(9),
          ],
        ),
      ),
    ]
    const result = await stylingTest.testCompile(ctx, style)

    test.assertDeepEquals(result.classNames, ['a0'])
    test.assertDeepEquals(result.code, [
      stylingTest.dedent(`
        .a0 {
          background-image: linear-gradient(1deg, rgb(0% 0% 2%), rgb(0% 0% 3%)),
            linear-gradient(4deg, rgb(0% 0% 5%), 6%, rgb(0% 0% 7%) 8% 9%);
        }
      `),
    ])
  },

  async ['size / multi'](ctx: styling.Context): Promise<void> {
    const style = [
      styling.background.size('cover', [styling.px(1), 'auto']),
    ]
    const result = await stylingTest.testCompile(ctx, style)

    test.assertDeepEquals(result.classNames, ['a0'])
    test.assertDeepEquals(result.code, [
      stylingTest.dedent(`
        .a0 {
          background-size:
            cover,
            1px auto;
        }
      `),
    ])
  },
}
