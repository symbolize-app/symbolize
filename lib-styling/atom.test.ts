import * as styling from '@/index.ts'
import * as stylingTest from '@/test.ts'
import * as test from '@symbolize/lib-test'

export const url = import.meta.url

export const tests = {
  async ['compile basic'](ctx: styling.Context): Promise<void> {
    for (let i = 0; i < 2; i += 1) {
      const style = [
        styling.background.color(
          styling.rgb(styling.pct(100), styling.pct(0), styling.pct(0)),
        ),
      ]
      const result = await stylingTest.testCompile(ctx, style)

      test.assertDeepEquals(result.classNames, ['a0'])
      test.assertDeepEquals(result.code, [
        stylingTest.dedent(`
          .a0 {
            background-color: rgb(100% 0% 0%);
          }
        `),
      ])
    }
  },

  async ['atom override'](ctx: styling.Context): Promise<void> {
    const style = [
      styling.background.color(
        styling.rgb(styling.pct(0), styling.pct(0), styling.pct(1)),
      ),
      styling.background.color(
        styling.rgb(styling.pct(0), styling.pct(0), styling.pct(2)),
      ),
    ]
    const result = await stylingTest.testCompile(ctx, style)

    test.assertDeepEquals(result.classNames, ['a0'])
    test.assertDeepEquals(result.code, [
      stylingTest.dedent(`
        .a0 {
          background-color: rgb(0% 0% 2%);
        }
      `),
    ])
  },

  async ['atom pseudo-element override'](
    ctx: styling.Context,
  ): Promise<void> {
    const style = [
      styling.background.color(
        styling.rgb(styling.pct(0), styling.pct(0), styling.pct(1)),
      ),
      styling.background.color(
        styling.rgb(styling.pct(0), styling.pct(0), styling.pct(2)),
      ),
      styling.before([
        styling.background.color(
          styling.rgb(styling.pct(0), styling.pct(0), styling.pct(3)),
        ),
        styling.background.color(
          styling.rgb(styling.pct(0), styling.pct(0), styling.pct(4)),
        ),
      ]),
    ]
    const result = await stylingTest.testCompile(ctx, style)

    test.assertDeepEquals(result.classNames, ['a0', 'a1'])
    test.assertDeepEquals(result.code, [
      stylingTest.dedent(`
        .a0 {
          background-color: rgb(0% 0% 2%);
        }
      `),
      stylingTest.dedent(`
        .a1::before {
          background-color: rgb(0% 0% 4%);
        }
      `),
    ])
  },

  async ['atom nesting'](ctx: styling.Context): Promise<void> {
    const style = [
      [
        styling.fill(
          styling.rgb(styling.pct(0), styling.pct(0), styling.pct(1)),
        ),
        styling.background.color(
          styling.rgb(styling.pct(0), styling.pct(0), styling.pct(2)),
        ),
      ],
    ]
    const result = await stylingTest.testCompile(ctx, style)

    test.assertDeepEquals(result.classNames, ['a0', 'a1'])
    test.assertDeepEquals(result.code, [
      stylingTest.dedent(`
        .a0 {
          fill: rgb(0% 0% 1%);
        }
      `),
      stylingTest.dedent(`
        .a1 {
          background-color: rgb(0% 0% 2%);
        }
      `),
    ])
  },
}
