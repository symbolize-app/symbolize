import * as contrast from '@/index.ts'
import * as contrastTest from '@/test.ts'
import * as test from '@symbolize/lib-test'

export const url = import.meta.url

export const tests = {
  async ['compile basic'](ctx: contrast.Context): Promise<void> {
    for (let i = 0; i < 2; i += 1) {
      const style = [
        contrast.background.color(
          contrast.rgb(
            contrast.pct(100),
            contrast.pct(0),
            contrast.pct(0),
          ),
        ),
      ]
      const result = await contrastTest.testCompile(ctx, style)

      test.assertDeepEquals(result.classNames, ['a0'])
      test.assertDeepEquals(result.code, [
        contrastTest.dedent(`
          .a0 {
            background-color: rgb(100% 0% 0%);
          }
        `),
      ])
    }
  },

  async ['atom override'](ctx: contrast.Context): Promise<void> {
    const style = [
      contrast.background.color(
        contrast.rgb(contrast.pct(0), contrast.pct(0), contrast.pct(1)),
      ),
      contrast.background.color(
        contrast.rgb(contrast.pct(0), contrast.pct(0), contrast.pct(2)),
      ),
    ]
    const result = await contrastTest.testCompile(ctx, style)

    test.assertDeepEquals(result.classNames, ['a0'])
    test.assertDeepEquals(result.code, [
      contrastTest.dedent(`
        .a0 {
          background-color: rgb(0% 0% 2%);
        }
      `),
    ])
  },

  async ['atom pseudo-element override'](
    ctx: contrast.Context,
  ): Promise<void> {
    const style = [
      contrast.background.color(
        contrast.rgb(contrast.pct(0), contrast.pct(0), contrast.pct(1)),
      ),
      contrast.background.color(
        contrast.rgb(contrast.pct(0), contrast.pct(0), contrast.pct(2)),
      ),
      contrast.before([
        contrast.background.color(
          contrast.rgb(contrast.pct(0), contrast.pct(0), contrast.pct(3)),
        ),
        contrast.background.color(
          contrast.rgb(contrast.pct(0), contrast.pct(0), contrast.pct(4)),
        ),
      ]),
    ]
    const result = await contrastTest.testCompile(ctx, style)

    test.assertDeepEquals(result.classNames, ['a0', 'a1'])
    test.assertDeepEquals(result.code, [
      contrastTest.dedent(`
        .a0 {
          background-color: rgb(0% 0% 2%);
        }
      `),
      contrastTest.dedent(`
        .a1::before {
          background-color: rgb(0% 0% 4%);
        }
      `),
    ])
  },

  async ['atom nesting'](ctx: contrast.Context): Promise<void> {
    const style = [
      [
        contrast.fill(
          contrast.rgb(contrast.pct(0), contrast.pct(0), contrast.pct(1)),
        ),
        contrast.background.color(
          contrast.rgb(contrast.pct(0), contrast.pct(0), contrast.pct(2)),
        ),
      ],
    ]
    const result = await contrastTest.testCompile(ctx, style)

    test.assertDeepEquals(result.classNames, ['a0', 'a1'])
    test.assertDeepEquals(result.code, [
      contrastTest.dedent(`
        .a0 {
          fill: rgb(0% 0% 1%);
        }
      `),
      contrastTest.dedent(`
        .a1 {
          background-color: rgb(0% 0% 2%);
        }
      `),
    ])
  },
}
