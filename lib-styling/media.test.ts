import * as styling from '@/index.ts'
import * as stylingTest from '@/test.ts'
import * as test from '@symbolize/lib-test'

export const url = import.meta.url

export const tests = {
  async ['hint / reduced motion / basic'](
    ctx: styling.Context,
  ): Promise<void> {
    const style = [
      styling.overflow.x(
        styling.media.match(
          'all',
          styling.media.hint.reducedMotion(),
          'scroll',
        ),
      ),
    ]
    const result = await stylingTest.testCompile(ctx, style)

    test.assertDeepEquals(result.classNames, ['a0'])
    test.assertDeepEquals(result.code, [
      stylingTest.dedent(`
        .a0 {
          @media only all and (prefers-reduced-motion: reduce) {
            overflow-x: scroll;
          }
        }
      `),
    ])
  },
  async ['size / basic'](ctx: styling.Context): Promise<void> {
    const style = [
      styling.overflow.x(
        styling.media.match(
          'screen',
          styling.media.min.w(styling.px(300)),
          'scroll',
        ),
      ),
    ]
    const result = await stylingTest.testCompile(ctx, style)

    test.assertDeepEquals(result.classNames, ['a0'])
    test.assertDeepEquals(result.code, [
      stylingTest.dedent(`
        .a0 {
          @media only screen and (min-width: 300px) {
            overflow-x: scroll;
          }
        }
      `),
    ])
  },

  async ['size / calc'](ctx: styling.Context): Promise<void> {
    const style = [
      styling.overflow.x(
        styling.media.match(
          'screen',
          styling.media.min.w(
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
          @media only screen and (min-width: calc(30rem + 300px)) {
            overflow-x: scroll;
          }
        }
      `),
    ])
  },

  async ['size / extra rules'](ctx: styling.Context): Promise<void> {
    const style = [
      styling.overflow.x(
        styling.media.match(
          'all',
          styling.media.min.h(styling.c(styling.px(100), styling.px(200))),
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

  async ['hover / basic'](ctx: styling.Context): Promise<void> {
    const style = [
      styling.overflow.x(
        styling.media.match('all', styling.media.hover(), 'scroll'),
      ),
    ]
    const result = await stylingTest.testCompile(ctx, style)

    test.assertDeepEquals(result.classNames, ['a0'])
    test.assertDeepEquals(result.code, [
      stylingTest.dedent(`
        .a0 {
          @media only all and (hover: hover) {
            overflow-x: scroll;
          }
        }
      `),
    ])
  },

  async ['and'](ctx: styling.Context): Promise<void> {
    const style = [
      styling.overflow.x(
        styling.media.match(
          'screen',
          styling.media.and(
            styling.media.hint.reducedMotion('reduce'),
            styling.media.orientation('portrait'),
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
          @media only screen and ((prefers-reduced-motion: reduce) and (orientation: portrait)) {
            overflow-x: scroll;
          }
        }
      `),
    ])
  },

  async ['not'](ctx: styling.Context): Promise<void> {
    const style = [
      styling.overflow.x(
        styling.media.match(
          'screen',
          styling.media.not(
            styling.media.hint.reducedMotion('reduce'),
            styling.media.orientation('portrait'),
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
          @media only screen and (not ((prefers-reduced-motion: reduce) and (orientation: portrait))) {
            overflow-x: scroll;
          }
        }
      `),
    ])
  },

  async ['or'](ctx: styling.Context): Promise<void> {
    const style = [
      styling.overflow.x(
        styling.media.match(
          'screen',
          styling.media.or(
            styling.media.hint.reducedMotion('reduce'),
            styling.media.orientation('portrait'),
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
          @media only screen and ((prefers-reduced-motion: reduce) or (orientation: portrait)) {
            overflow-x: scroll;
          }
        }
      `),
    ])
  },
}
