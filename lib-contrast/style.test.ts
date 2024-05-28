import * as contrast from '@/index.ts'
import * as contrastTest from '@/test.ts'
import * as compute from '@intertwine/lib-compute'
import * as test from '@intertwine/lib-test'

export const url = import.meta.url

export const tests = {
  async ['compile basic'](ctx: contrast.Context): Promise<void> {
    for (let i = 0; i < 2; i += 1) {
      const style = [contrast.background.color(contrast.rgb(255, 0, 0))]
      const result = await contrastTest.testCompile(ctx, style)

      test.assertDeepEquals(result.classNames, ['a0'])
      test.assertDeepEquals(result.code, [
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
      contrast.background.color(contrast.rgb(0, 0, 1)),
      contrast.background.color(contrast.rgb(0, 0, 2)),
    ]
    const result = await contrastTest.testCompile(ctx, style)

    test.assertDeepEquals(result.classNames, ['a0'])
    test.assertDeepEquals(result.code, [
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
        contrast.fill(contrast.rgb(0, 0, 1)),
        contrast.background.color(contrast.rgb(0, 0, 2)),
      ],
    ]
    const result = await contrastTest.testCompile(ctx, style)

    test.assertDeepEquals(result.classNames, ['a0', 'a1'])
    test.assertDeepEquals(result.code, [
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

  async ['stable computation custom property names'](
    ctx: contrast.Context,
  ): Promise<void> {
    for (let i = 0; i < 2; i += 1) {
      const a = compute.pure(1)
      const b = compute.pure(2)
      const c = compute.pure(3)
      const style = [
        contrast.fill(contrast.rgb(0, 0, a)),
        contrast.background.color(contrast.rgb(0, 0, b)),
        contrast.background.color(
          contrast.rgb(0, a, [0, contrast.hover(c)]),
        ),
      ]
      const result = await contrastTest.testCompile(ctx, style)

      test.assertDeepEquals(result.classNames, ['a0', 'e0', 'a1'])
      test.assertDeepEquals(result.code, [
        contrastTest.dedent(`
          .a0 {
            fill: rgb(0, 0, var(--c0));
          }
        `),
        contrastTest.dedent(`
          .e0 {
            --e0: 0;
            &:where(:hover) {
              --e0: var(--c1);
            }
          }
        `),
        contrastTest.dedent(`
          .a1 {
            background-color: rgb(0, var(--c0), var(--e0));
          }
        `),
      ])
      test.assertDeepEquals(result.computationCustomProperties, [
        [a, '--c0'],
        [c, '--c1'],
      ])
    }
  },

  async ['computation expression custom property'](
    ctx: contrast.Context,
  ): Promise<void> {
    const a = compute.pure(contrast.rgb(0, 0, 1))
    const style = [contrast.background.color(a)]
    const result = await contrastTest.testCompile(ctx, style)

    test.assertDeepEquals(result.classNames, ['a0'])
    test.assertDeepEquals(result.code, [
      contrastTest.dedent(`
        .a0 {
          background-color: var(--c0);
        }
      `),
    ])
    test.assertDeepEquals(result.computationCustomProperties, [
      [a, '--c0'],
    ])
  },
}
