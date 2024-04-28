import * as contrast from '@/index.ts'
import * as contrastTest from '@/test.ts'
import * as test from '@intertwine/lib-test'

export const url = import.meta.url

export const tests = {
  async ['add'](ctx: contrast.Context): Promise<void> {
    const atom = contrast.background.color(
      contrast.rgb(0, 0, contrast.add(1, 2)),
    )
    const rules = [...atom.compile(ctx).rules(ctx)]

    test.assertDeepEquals(contrastTest.ruleClassNames(rules), ['a0'])
    test.assertDeepEquals(await contrastTest.ruleCode(rules), [
      contrastTest.dedent(`
        .a0 {
          background-color: rgb(0, 0, calc(1+2));
        }
      `),
    ])
  },
}
