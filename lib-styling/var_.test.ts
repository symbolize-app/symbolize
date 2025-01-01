import * as styling from '@/index.ts'
import * as stylingTest from '@/test.ts'
import * as test from '@symbolize/lib-test'

export const url = import.meta.url

export const tests = {
  async ['stable names'](ctx: styling.Context): Promise<void> {
    const a = styling.var_<styling.Pct>()
    const b = styling.var_<styling.Pct>()
    for (let i = 0; i < 2; i += 1) {
      const style = [
        a.set(styling.pct(1)),
        b.set(styling.pct(2)),
        styling.background.color(styling.rgb(styling.pct(0), a, b)),
      ]
      const result = await stylingTest.testCompile(ctx, style)

      test.assertDeepEquals(result.classNames, ['a0', 'a1', 'a2'])
      test.assertDeepEquals(result.code, [
        stylingTest.dedent(`
          .a0 {
            --s0: 1%;
          }
        `),
        stylingTest.dedent(`
          .a1 {
            --s1: 2%;
          }
        `),
        stylingTest.dedent(`
          .a2 {
            background-color: rgb(0% var(--s0) var(--s1));
          }
        `),
      ])
    }
  },

  async ['get or default'](ctx: styling.Context): Promise<void> {
    const a = styling.var_<styling.Pct>()
    const style = [
      styling.background.color(
        styling.rgb(
          styling.pct(0),
          styling.pct(0),
          a.or(
            styling.c(
              styling.pct(0),
              styling.select.match(styling.select.hover(), styling.pct(1)),
            ),
          ),
        ),
      ),
    ]
    const result = await stylingTest.testCompile(ctx, style)

    test.assertDeepEquals(result.classNames, ['e0', 'a0'])
    test.assertDeepEquals(result.code, [
      stylingTest.dedent(`
        .e0 {
          --e0: 0%;
          &:where(:hover) {
            --e0: 1%;
          }
        }
      `),
      stylingTest.dedent(`
        .a0 {
          background-color: rgb(0% 0% var(--s0, var(--e0)));
        }
      `),
    ])
  },

  ['resolve'](ctx: styling.Context): void {
    const a = styling.var_<number>()
    test.assertEquals(a.resolve(ctx), '--s0')
  },
}
