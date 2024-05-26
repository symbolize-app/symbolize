import * as contrast from '@/index.ts'
import * as contrastTest from '@/test.ts'
import * as test from '@intertwine/lib-test'

export const url = import.meta.url

export const tests = {
  async ['hover'](ctx: contrast.Context): Promise<void> {
    const atom = contrast.background.color(
      contrast.hover(contrast.rgb(255, 0, 128)),
    )
    const rules = [...atom.compile(ctx).rules(ctx)]

    test.assertDeepEquals(contrastTest.ruleClassNames(rules), ['a0'])
    test.assertDeepEquals(await contrastTest.ruleCode(rules), [
      contrastTest.dedent(`
        .a0 {
          &:where(:hover) {
            background-color: rgb(255, 0, 128);
          }
        }
      `),
    ])
  },
}
