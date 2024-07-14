import * as contrast from '@/index.ts'
import * as contrastTest from '@/test.ts'
import * as test from '@intertwine/lib-test'

export const url = import.meta.url

export const tests = {
  async ['stable names'](ctx: contrast.Context): Promise<void> {
    const a = contrast.var_<contrast.Pct>()
    const b = contrast.var_<contrast.Pct>()
    for (let i = 0; i < 2; i += 1) {
      const style = [
        a.set(contrast.pct(1)),
        b.set(contrast.pct(2)),
        contrast.background.color(contrast.rgb(contrast.pct(0), a, b)),
      ]
      const result = await contrastTest.testCompile(ctx, style)

      test.assertDeepEquals(result.classNames, ['a0', 'a1', 'a2'])
      test.assertDeepEquals(result.code, [
        contrastTest.dedent(`
          .a0 {
            --s0: 1%;
          }
        `),
        contrastTest.dedent(`
          .a1 {
            --s1: 2%;
          }
        `),
        contrastTest.dedent(`
          .a2 {
            background-color: rgb(0% var(--s0) var(--s1));
          }
        `),
      ])
    }
  },

  async ['get or default'](ctx: contrast.Context): Promise<void> {
    const a = contrast.var_<contrast.Pct>()
    const style = [
      contrast.background.color(
        contrast.rgb(
          contrast.pct(0),
          contrast.pct(0),
          a.or(
            contrast.c(
              contrast.pct(0),
              contrast.select.match(
                contrast.select.hover(),
                contrast.pct(1),
              ),
            ),
          ),
        ),
      ),
    ]
    const result = await contrastTest.testCompile(ctx, style)

    test.assertDeepEquals(result.classNames, ['e0', 'a0'])
    test.assertDeepEquals(result.code, [
      contrastTest.dedent(`
        .e0 {
          --e0: 0%;
          &:where(:hover) {
            --e0: 1%;
          }
        }
      `),
      contrastTest.dedent(`
        .a0 {
          background-color: rgb(0% 0% var(--s0, var(--e0)));
        }
      `),
    ])
  },

  ['resolve'](ctx: contrast.Context): void {
    const a = contrast.var_<number>()
    test.assertEquals(a.resolve(ctx), '--s0')
  },
}
