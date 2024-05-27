import * as contrast from '@/index.ts'
import * as contrastTest from '@/test.ts'
import * as test from '@intertwine/lib-test'

export const url = import.meta.url

export const tests = {
  async ['stable names'](ctx: contrast.Context): Promise<void> {
    const a = contrast.customProperty<number>()
    const b = contrast.customProperty<number>()
    for (let i = 0; i < 2; i += 1) {
      const style = [
        a.set(1),
        b.set(2),
        contrast.background.color([contrast.rgb(0, a.get(), b.get())]),
      ]
      const rules = [...contrast.compile(ctx, style)]

      test.assertDeepEquals(contrastTest.ruleClassNames(rules), [
        'a0',
        'a1',
        'a2',
      ])
      test.assertDeepEquals(await contrastTest.ruleCode(rules), [
        contrastTest.dedent(`
          .a0 {
            --s0: 1;
          }
        `),
        contrastTest.dedent(`
          .a1 {
            --s1: 2;
          }
        `),
        contrastTest.dedent(`
          .a2 {
            background-color: rgb(0, var(--s0), var(--s1));
          }
        `),
      ])
    }
  },

  async ['get or default'](ctx: contrast.Context): Promise<void> {
    const a = contrast.customProperty<number>()
    const style = [
      contrast.background.color([
        contrast.rgb(0, 0, a.getOr([0, contrast.hover(1)])),
      ]),
    ]
    const rules = [...contrast.compile(ctx, style)]

    test.assertDeepEquals(contrastTest.ruleClassNames(rules), ['e0', 'a0'])
    test.assertDeepEquals(await contrastTest.ruleCode(rules), [
      contrastTest.dedent(`
        .e0 {
          --e0: 0;
          &:where(:hover) {
            --e0: 1;
          }
        }
      `),
      contrastTest.dedent(`
        .a0 {
          background-color: rgb(0, 0, var(--s0, var(--e0)));
        }
      `),
    ])
  },
}
