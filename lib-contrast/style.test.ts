import * as contrast from '@/index.ts'
import * as contrastTest from '@/test.ts'
import * as test from '@intertwine/lib-test'

export const url = import.meta.url

export const tests = {
  async ['compile basic'](ctx: contrast.Context): Promise<void> {
    for (let i = 0; i < 2; i += 1) {
      const style = [contrast.background.color([contrast.rgb(255, 0, 0)])]
      const rules = [...contrast.compile(ctx, style)]

      test.assertDeepEquals(contrastTest.ruleClassNames(rules), ['a0'])
      test.assertDeepEquals(await contrastTest.ruleCode(rules), [
        contrastTest.dedent(`
          .a0 {
            background-color: rgb(255, 0, 0);
          }
        `),
      ])
    }
  },

  async ['atom override'](ctx: contrast.Context): Promise<void> {
    const style = [
      contrast.background.color([contrast.rgb(0, 0, 1)]),
      contrast.background.color([contrast.rgb(0, 0, 2)]),
    ]
    const rules = [...contrast.compile(ctx, style)]

    test.assertDeepEquals(contrastTest.ruleClassNames(rules), ['a0'])
    test.assertDeepEquals(await contrastTest.ruleCode(rules), [
      contrastTest.dedent(`
        .a0 {
          background-color: rgb(0, 0, 2);
        }
      `),
    ])
  },

  async ['atom nesting'](ctx: contrast.Context): Promise<void> {
    const style = [
      [
        contrast.fill([contrast.rgb(0, 0, 1)]),
        contrast.background.color([contrast.rgb(0, 0, 2)]),
      ],
    ]
    const rules = [...contrast.compile(ctx, style)]

    test.assertDeepEquals(contrastTest.ruleClassNames(rules), ['a0', 'a1'])
    test.assertDeepEquals(await contrastTest.ruleCode(rules), [
      contrastTest.dedent(`
        .a0 {
          fill: rgb(0, 0, 1);
        }
      `),
      contrastTest.dedent(`
        .a1 {
          background-color: rgb(0, 0, 2);
        }
      `),
    ])
  },
}
