import * as contrast from '@/index.ts'
import * as contrastTest from '@/test.ts'
import * as test from '@symbolize/lib-test'

export const url = import.meta.url

export const tests = {
  async ['hint / reduced motion / basic'](
    ctx: contrast.Context,
  ): Promise<void> {
    const style = [
      contrast.overflow.x(
        contrast.media.match(
          'all',
          contrast.media.hint.reducedMotion(),
          'scroll',
        ),
      ),
    ]
    const result = await contrastTest.testCompile(ctx, style)

    test.assertDeepEquals(result.classNames, ['a0'])
    test.assertDeepEquals(result.code, [
      contrastTest.dedent(`
        .a0 {
          @media only all and (prefers-reduced-motion: reduce) {
            overflow-x: scroll;
          }
        }
      `),
    ])
  },
  async ['size / basic'](ctx: contrast.Context): Promise<void> {
    const style = [
      contrast.overflow.x(
        contrast.media.match(
          'screen',
          contrast.media.min.w(contrast.px(300)),
          'scroll',
        ),
      ),
    ]
    const result = await contrastTest.testCompile(ctx, style)

    test.assertDeepEquals(result.classNames, ['a0'])
    test.assertDeepEquals(result.code, [
      contrastTest.dedent(`
        .a0 {
          @media only screen and (min-width: 300px) {
            overflow-x: scroll;
          }
        }
      `),
    ])
  },

  async ['size / calc'](ctx: contrast.Context): Promise<void> {
    const style = [
      contrast.overflow.x(
        contrast.media.match(
          'screen',
          contrast.media.min.w(
            contrast.add(contrast.rem(30), contrast.px(300)),
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
          @media only screen and (min-width: calc(30rem + 300px)) {
            overflow-x: scroll;
          }
        }
      `),
    ])
  },

  async ['size / extra rules'](ctx: contrast.Context): Promise<void> {
    const style = [
      contrast.overflow.x(
        contrast.media.match(
          'all',
          contrast.media.min.h(
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

  async ['hover / basic'](ctx: contrast.Context): Promise<void> {
    const style = [
      contrast.overflow.x(
        contrast.media.match('all', contrast.media.hover(), 'scroll'),
      ),
    ]
    const result = await contrastTest.testCompile(ctx, style)

    test.assertDeepEquals(result.classNames, ['a0'])
    test.assertDeepEquals(result.code, [
      contrastTest.dedent(`
        .a0 {
          @media only all and (hover: hover) {
            overflow-x: scroll;
          }
        }
      `),
    ])
  },

  async ['and'](ctx: contrast.Context): Promise<void> {
    const style = [
      contrast.overflow.x(
        contrast.media.match(
          'screen',
          contrast.media.and(
            contrast.media.hint.reducedMotion('reduce'),
            contrast.media.orientation('portrait'),
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
          @media only screen and ((prefers-reduced-motion: reduce) and (orientation: portrait)) {
            overflow-x: scroll;
          }
        }
      `),
    ])
  },

  async ['not'](ctx: contrast.Context): Promise<void> {
    const style = [
      contrast.overflow.x(
        contrast.media.match(
          'screen',
          contrast.media.not(
            contrast.media.hint.reducedMotion('reduce'),
            contrast.media.orientation('portrait'),
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
          @media only screen and (not ((prefers-reduced-motion: reduce) and (orientation: portrait))) {
            overflow-x: scroll;
          }
        }
      `),
    ])
  },

  async ['or'](ctx: contrast.Context): Promise<void> {
    const style = [
      contrast.overflow.x(
        contrast.media.match(
          'screen',
          contrast.media.or(
            contrast.media.hint.reducedMotion('reduce'),
            contrast.media.orientation('portrait'),
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
          @media only screen and ((prefers-reduced-motion: reduce) or (orientation: portrait)) {
            overflow-x: scroll;
          }
        }
      `),
    ])
  },
}
