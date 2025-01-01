import * as styling from '@/index.ts'
import * as stylingTest from '@/test.ts'
import * as test from '@symbolize/lib-test'

export const url = import.meta.url

export const tests = {
  async ['name / stable'](ctx: styling.Context): Promise<void> {
    const a = styling.container.build()
    const b = styling.container.build()

    for (let i = 0; i < 2; i++) {
      const style = [
        styling.container.name(a, b),
        styling.overflow.x(
          styling.container.match(
            a,
            styling.container.orientation('portrait'),
            'scroll',
          ),
        ),
      ]
      const result = await stylingTest.testCompile(ctx, style)

      test.assertDeepEquals(result.classNames, ['a0', 'a1'])
      test.assertDeepEquals(result.code, [
        stylingTest.dedent(`
          .a0 {
            container-name: r0 r1;
          }
        `),
        stylingTest.dedent(`
          .a1 {
            @container r0 (orientation: portrait) {
              overflow-x: scroll;
            }
          }
        `),
      ])
    }
  },

  async ['type / basic'](ctx: styling.Context): Promise<void> {
    const style = [styling.container.type('size')]
    const code = 'container-type: size'
    await stylingTest.assertCompileBasicEquals(ctx, style, code)
  },

  async ['orientation / basic'](ctx: styling.Context): Promise<void> {
    const style = [
      styling.overflow.x(
        styling.container.match(
          'all',
          styling.container.orientation('portrait'),
          'scroll',
        ),
      ),
    ]
    const result = await stylingTest.testCompile(ctx, style)

    test.assertDeepEquals(result.classNames, ['a0'])
    test.assertDeepEquals(result.code, [
      stylingTest.dedent(`
        .a0 {
          @container (orientation: portrait) {
            overflow-x: scroll;
          }
        }
      `),
    ])
  },

  async ['size / basic'](ctx: styling.Context): Promise<void> {
    const style = [
      styling.overflow.x(
        styling.container.match(
          'all',
          styling.container.min.w(styling.px(300)),
          'scroll',
        ),
      ),
    ]
    const result = await stylingTest.testCompile(ctx, style)

    test.assertDeepEquals(result.classNames, ['a0'])
    test.assertDeepEquals(result.code, [
      stylingTest.dedent(`
        .a0 {
          @container (min-width: 300px) {
            overflow-x: scroll;
          }
        }
      `),
    ])
  },

  async ['size / calc'](ctx: styling.Context): Promise<void> {
    const style = [
      styling.overflow.x(
        styling.container.match(
          'all',
          styling.container.min.w(
            styling.add(styling.rem(30), styling.px(300)),
          ),
          'scroll',
        ),
      ),
    ]
    const result = await stylingTest.testCompile(ctx, style)

    test.assertDeepEquals(result.classNames, ['a0'])
    test.assertDeepEquals(result.code, [
      stylingTest.dedent(`
        .a0 {
          @container (min-width: calc(30rem + 300px)) {
            overflow-x: scroll;
          }
        }
      `),
    ])
  },

  async ['size / extra rules'](ctx: styling.Context): Promise<void> {
    const style = [
      styling.overflow.x(
        styling.container.match(
          'all',
          styling.container.min.h(
            styling.c(styling.px(100), styling.px(200)),
          ),
          'scroll',
        ),
      ),
    ]
    const error = await test.assertThrowsAsync(async () =>
      stylingTest.testCompile(ctx, style),
    )

    test.assertInstanceOf(error, Error)
    test.assertEquals(
      error.message,
      'Extra rules not allowed for scope conditions',
    )
  },

  async ['and'](ctx: styling.Context): Promise<void> {
    const style = [
      styling.overflow.x(
        styling.container.match(
          'all',
          styling.container.and(
            styling.container.min.o(styling.px(400)),
            styling.container.orientation('portrait'),
          ),
          'scroll',
        ),
      ),
    ]
    const result = await stylingTest.testCompile(ctx, style)

    test.assertDeepEquals(result.classNames, ['a0'])
    test.assertDeepEquals(result.code, [
      stylingTest.dedent(`
        .a0 {
          @container ((min-block-size: 400px) and (orientation: portrait)) {
            overflow-x: scroll;
          }
        }
      `),
    ])
  },

  async ['not'](ctx: styling.Context): Promise<void> {
    const style = [
      styling.overflow.x(
        styling.container.match(
          'all',
          styling.container.not(
            styling.container.min.o(styling.px(400)),
            styling.container.orientation('portrait'),
          ),
          'scroll',
        ),
      ),
    ]
    const result = await stylingTest.testCompile(ctx, style)

    test.assertDeepEquals(result.classNames, ['a0'])
    test.assertDeepEquals(result.code, [
      stylingTest.dedent(`
        .a0 {
          @container (not ((min-block-size: 400px) and (orientation: portrait))) {
            overflow-x: scroll;
          }
        }
      `),
    ])
  },

  async ['or'](ctx: styling.Context): Promise<void> {
    const style = [
      styling.overflow.x(
        styling.container.match(
          'all',
          styling.container.or(
            styling.container.min.o(styling.px(400)),
            styling.container.orientation('portrait'),
          ),
          'scroll',
        ),
      ),
    ]
    const result = await stylingTest.testCompile(ctx, style)

    test.assertDeepEquals(result.classNames, ['a0'])
    test.assertDeepEquals(result.code, [
      stylingTest.dedent(`
        .a0 {
          @container ((min-block-size: 400px) or (orientation: portrait)) {
            overflow-x: scroll;
          }
        }
      `),
    ])
  },
}
