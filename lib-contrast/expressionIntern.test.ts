import * as contrast from '@/index.ts'
import * as contrastTest from '@/test.ts'
import * as test from '@intertwine/lib-test'

export const url = import.meta.url

export const tests = {
  async ['null'](ctx: contrast.Context): Promise<void> {
    const atom = contrast.background.color([])
    const rules = [...atom.compile(ctx).rules(ctx)]

    test.assertDeepEquals(contrastTest.ruleClassNames(rules), ['a0'])
    test.assertDeepEquals(await contrastTest.ruleCode(rules), [
      contrastTest.dedent(`
        .a0 {
        }
      `),
    ])
  },

  async ['multi semicolon complex'](ctx: contrast.Context): Promise<void> {
    const atom = contrast.background.color([
      contrast.rgb(0, 0, 0),
      [contrast.rgb(0, 0, 1), contrast.rgb(0, 0, 2)],
      contrast.rgb(0, 0, 3),
      contrast.hover([
        contrast.rgb(0, 0, 4),
        [contrast.rgb(0, 0, 5), contrast.rgb(0, 0, 6)],
        contrast.rgb(0, 0, 7),
      ]),
      contrast.rgb(0, 0, 8),
      contrast.rgb(0, 0, 9),
    ])
    const rules = [...atom.compile(ctx).rules(ctx)]

    test.assertDeepEquals(contrastTest.ruleClassNames(rules), ['a0'])
    test.assertDeepEquals(await contrastTest.ruleCode(rules), [
      contrastTest.dedent(`
        .a0 {
          background-color: rgb(0, 0, 0);
          background-color: rgb(0, 0, 1);
          background-color: rgb(0, 0, 2);
          background-color: rgb(0, 0, 3);
          &:where(:hover) {
            background-color: rgb(0, 0, 4);
            background-color: rgb(0, 0, 5);
            background-color: rgb(0, 0, 6);
            background-color: rgb(0, 0, 7);
          }
          & {
            background-color: rgb(0, 0, 8);
            background-color: rgb(0, 0, 9);
          }
        }
      `),
    ])
  },

  async ['scope expression reuse'](ctx: contrast.Context): Promise<void> {
    const atom = contrast.background.color(
      contrast.rgb(0, [0, contrast.hover(255)], [0, contrast.hover(255)]),
    )
    const rules = [...new Set(atom.compile(ctx).rules(ctx))]

    test.assertDeepEquals(contrastTest.ruleClassNames(rules), ['e0', 'a0'])
    test.assertDeepEquals(await contrastTest.ruleCode(rules), [
      contrastTest.dedent(`
        .e0 {
          --e0: 0;
          &:where(:hover) {
            --e0: 255;
          }
        }
      `),
      contrastTest.dedent(`
        .a0 {
          background-color: rgb(0, var(--e0), var(--e0));
        }
      `),
    ])
  },

  async ['scope expression chaining'](
    ctx: contrast.Context,
  ): Promise<void> {
    const atom = contrast.background.color(
      contrast.rgb(0, 0, [
        0,
        contrast.hover(contrast.add(1, [0, contrast.disabled(2)])),
      ]),
    )
    const rules = [...new Set(atom.compile(ctx).rules(ctx))]

    test.assertDeepEquals(contrastTest.ruleClassNames(rules), [
      'e0',
      'e1',
      'a0',
    ])
    test.assertDeepEquals(await contrastTest.ruleCode(rules), [
      contrastTest.dedent(`
        .e0 {
          --e0: 0;
          &:where(:disabled) {
            --e0: 2;
          }
        }
      `),
      contrastTest.dedent(`
        .e1 {
          --e1: 0;
          &:where(:hover) {
            --e1: calc(1+var(--e0));
          }
        }
      `),
      contrastTest.dedent(`
        .a0 {
          background-color: rgb(0, 0, var(--e1));
        }
      `),
    ])
  },
}
