import * as contrast from '@/index.ts'
import * as contrastTest from '@/test.ts'
import * as test from '@intertwine/lib-test'

export const url = import.meta.url

export const tests = {
  async ['name / stable'](ctx: contrast.Context): Promise<void> {
    const a = contrast.container.build()
    const b = contrast.container.build()

    for (let i = 0; i < 2; i++) {
      const style = [
        contrast.container.name(a, b),
        contrast.overflow.x(
          contrast.container.match(
            a,
            contrast.container.orientation('portrait'),
            'scroll',
          ),
        ),
      ]
      const result = await contrastTest.testCompile(ctx, style)

      test.assertDeepEquals(result.classNames, ['a0', 'a1'])
      test.assertDeepEquals(result.code, [
        contrastTest.dedent(`
          .a0 {
            container-name: r0 r1;
          }
        `),
        contrastTest.dedent(`
          .a1 {
            @container r0 (orientation: portrait) {
              overflow-x: scroll;
            }
          }
        `),
      ])
    }
  },

  async ['type / basic'](ctx: contrast.Context): Promise<void> {
    const style = [contrast.container.type('size')]
    const code = 'container-type: size'
    await contrastTest.assertCompileBasicEquals(ctx, style, code)
  },

  async ['orientation / basic'](ctx: contrast.Context): Promise<void> {
    const style = [
      contrast.overflow.x(
        contrast.container.match(
          'all',
          contrast.container.orientation('portrait'),
          'scroll',
        ),
      ),
    ]
    const result = await contrastTest.testCompile(ctx, style)

    test.assertDeepEquals(result.classNames, ['a0'])
    test.assertDeepEquals(result.code, [
      contrastTest.dedent(`
        .a0 {
          @container (orientation: portrait) {
            overflow-x: scroll;
          }
        }
      `),
    ])
  },

  async ['size / basic'](ctx: contrast.Context): Promise<void> {
    const style = [
      contrast.overflow.x(
        contrast.container.match(
          'all',
          contrast.container.min.w(contrast.px(300)),
          'scroll',
        ),
      ),
    ]
    const result = await contrastTest.testCompile(ctx, style)

    test.assertDeepEquals(result.classNames, ['a0'])
    test.assertDeepEquals(result.code, [
      contrastTest.dedent(`
        .a0 {
          @container (min-width: 300px) {
            overflow-x: scroll;
          }
        }
      `),
    ])
  },

  async ['size / calc'](ctx: contrast.Context): Promise<void> {
    const style = [
      contrast.overflow.x(
        contrast.container.match(
          'all',
          contrast.container.min.w(
            contrast.add<contrast.Length>(
              contrast.rem(30),
              contrast.px(300),
            ),
          ),
          'scroll',
        ),
      ),
    ]
    const result = await contrastTest.testCompile(ctx, style)

    test.assertDeepEquals(result.classNames, ['a0'])
    test.assertDeepEquals(result.code, [
      contrastTest.dedent(`
        .a0 {
          @container (min-width: calc(30rem + 300px)) {
            overflow-x: scroll;
          }
        }
      `),
    ])
  },

  async ['size / extra rules'](ctx: contrast.Context): Promise<void> {
    const style = [
      contrast.overflow.x(
        contrast.container.match(
          'all',
          contrast.container.min.h(
            contrast.c(contrast.px(100), contrast.px(200)),
          ),
          'scroll',
        ),
      ),
    ]
    const error = await test.assertThrowsAsync(async () =>
      contrastTest.testCompile(ctx, style),
    )

    test.assertInstanceOf(error, Error)
    test.assertEquals(
      error.message,
      'Extra rules not allowed for scope conditions',
    )
  },

  async ['and'](ctx: contrast.Context): Promise<void> {
    const style = [
      contrast.overflow.x(
        contrast.container.match(
          'all',
          contrast.container.and(
            contrast.container.min.o(contrast.px(400)),
            contrast.container.orientation('portrait'),
          ),
          'scroll',
        ),
      ),
    ]
    const result = await contrastTest.testCompile(ctx, style)

    test.assertDeepEquals(result.classNames, ['a0'])
    test.assertDeepEquals(result.code, [
      contrastTest.dedent(`
        .a0 {
          @container ((min-block-size: 400px) and (orientation: portrait)) {
            overflow-x: scroll;
          }
        }
      `),
    ])
  },

  async ['not'](ctx: contrast.Context): Promise<void> {
    const style = [
      contrast.overflow.x(
        contrast.container.match(
          'all',
          contrast.container.not(
            contrast.container.min.o(contrast.px(400)),
            contrast.container.orientation('portrait'),
          ),
          'scroll',
        ),
      ),
    ]
    const result = await contrastTest.testCompile(ctx, style)

    test.assertDeepEquals(result.classNames, ['a0'])
    test.assertDeepEquals(result.code, [
      contrastTest.dedent(`
        .a0 {
          @container (not ((min-block-size: 400px) and (orientation: portrait))) {
            overflow-x: scroll;
          }
        }
      `),
    ])
  },

  async ['or'](ctx: contrast.Context): Promise<void> {
    const style = [
      contrast.overflow.x(
        contrast.container.match(
          'all',
          contrast.container.or(
            contrast.container.min.o(contrast.px(400)),
            contrast.container.orientation('portrait'),
          ),
          'scroll',
        ),
      ),
    ]
    const result = await contrastTest.testCompile(ctx, style)

    test.assertDeepEquals(result.classNames, ['a0'])
    test.assertDeepEquals(result.code, [
      contrastTest.dedent(`
        .a0 {
          @container ((min-block-size: 400px) or (orientation: portrait)) {
            overflow-x: scroll;
          }
        }
      `),
    ])
  },
}
