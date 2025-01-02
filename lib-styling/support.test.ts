import * as styling from '@/index.ts'
import * as stylingTest from '@/test.ts'
import * as test from '@symbolize/lib-test'

export const url = import.meta.url

export const tests = {
  async ['code / basic'](ctx: styling.Context): Promise<void> {
    const style = [
      styling.overflow.x(
        styling.support.match(
          styling.support.code(styling.hyphen.mode('auto')),
          'scroll',
        ),
      ),
    ]
    const result = await stylingTest.testCompile(ctx, style)

    test.assertDeepEquals(result.classNames, ['a0'])
    test.assertDeepEquals(result.code, [
      stylingTest.dedent(`
        .a0 {
          @supports (hyphens: auto) {
            overflow-x: scroll;
          }
        }
      `),
    ])
  },

  async ['code / and'](ctx: styling.Context): Promise<void> {
    const style = [
      styling.overflow.x(
        styling.support.match(
          styling.support.code([
            styling.hyphen.mode('auto'),
            styling.hyphen.mode('manual'),
          ]),
          'scroll',
        ),
      ),
    ]
    const result = await stylingTest.testCompile(ctx, style)

    test.assertDeepEquals(result.classNames, ['a0'])
    test.assertDeepEquals(result.code, [
      stylingTest.dedent(`
        .a0 {
          @supports ((hyphens: auto) and (hyphens: manual)) {
            overflow-x: scroll;
          }
        }
      `),
    ])
  },

  async ['code / extra rules'](ctx: styling.Context): Promise<void> {
    const style = [
      styling.overflow.x(
        styling.support.match(
          styling.support.code(
            styling.hyphen.mode(styling.c('auto', 'manual')),
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
        styling.support.match(
          styling.support.and(
            styling.support.code(styling.hyphen.mode('auto')),
            styling.support.code(styling.hyphen.mode('manual')),
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
          @supports ((hyphens: auto) and (hyphens: manual)) {
            overflow-x: scroll;
          }
        }
      `),
    ])
  },

  async ['not'](ctx: styling.Context): Promise<void> {
    const style = [
      styling.overflow.x(
        styling.support.match(
          styling.support.not(
            styling.support.code(styling.hyphen.mode('auto')),
            styling.support.code(styling.hyphen.mode('manual')),
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
          @supports (not ((hyphens: auto) and (hyphens: manual))) {
            overflow-x: scroll;
          }
        }
      `),
    ])
  },

  async ['or'](ctx: styling.Context): Promise<void> {
    const style = [
      styling.overflow.x(
        styling.support.match(
          styling.support.or(
            styling.support.code(styling.hyphen.mode('auto')),
            styling.support.code(styling.hyphen.mode('manual')),
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
          @supports ((hyphens: auto) or (hyphens: manual)) {
            overflow-x: scroll;
          }
        }
      `),
    ])
  },
}
