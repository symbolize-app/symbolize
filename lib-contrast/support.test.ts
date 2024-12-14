import * as contrast from '@/index.ts'
import * as contrastTest from '@/test.ts'
import * as test from '@symbolize/lib-test'

export const url = import.meta.url

export const tests = {
  async ['code / basic'](ctx: contrast.Context): Promise<void> {
    const style = [
      contrast.overflow.x(
        contrast.support.match(
          contrast.support.code(contrast.hyphen.mode('auto')),
          'scroll',
        ),
      ),
    ]
    const result = await contrastTest.testCompile(ctx, style)

    test.assertDeepEquals(result.classNames, ['a0'])
    test.assertDeepEquals(result.code, [
      contrastTest.dedent(`
        .a0 {
          @supports (hyphens: auto) {
            overflow-x: scroll;
          }
        }
      `),
    ])
  },

  async ['code / and'](ctx: contrast.Context): Promise<void> {
    const style = [
      contrast.overflow.x(
        contrast.support.match(
          contrast.support.code([
            contrast.hyphen.mode('auto'),
            contrast.hyphen.mode('manual'),
          ]),
          'scroll',
        ),
      ),
    ]
    const result = await contrastTest.testCompile(ctx, style)

    test.assertDeepEquals(result.classNames, ['a0'])
    test.assertDeepEquals(result.code, [
      contrastTest.dedent(`
        .a0 {
          @supports ((hyphens: auto) and (hyphens: manual)) {
            overflow-x: scroll;
          }
        }
      `),
    ])
  },

  async ['code / extra rules'](ctx: contrast.Context): Promise<void> {
    const style = [
      contrast.overflow.x(
        contrast.support.match(
          contrast.support.code(
            contrast.hyphen.mode(contrast.c('auto', 'manual')),
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
        contrast.support.match(
          contrast.support.and(
            contrast.support.code(contrast.hyphen.mode('auto')),
            contrast.support.code(contrast.hyphen.mode('manual')),
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
          @supports ((hyphens: auto) and (hyphens: manual)) {
            overflow-x: scroll;
          }
        }
      `),
    ])
  },

  async ['not'](ctx: contrast.Context): Promise<void> {
    const style = [
      contrast.overflow.x(
        contrast.support.match(
          contrast.support.not(
            contrast.support.code(contrast.hyphen.mode('auto')),
            contrast.support.code(contrast.hyphen.mode('manual')),
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
          @supports (not ((hyphens: auto) and (hyphens: manual))) {
            overflow-x: scroll;
          }
        }
      `),
    ])
  },

  async ['or'](ctx: contrast.Context): Promise<void> {
    const style = [
      contrast.overflow.x(
        contrast.support.match(
          contrast.support.or(
            contrast.support.code(contrast.hyphen.mode('auto')),
            contrast.support.code(contrast.hyphen.mode('manual')),
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
          @supports ((hyphens: auto) or (hyphens: manual)) {
            overflow-x: scroll;
          }
        }
      `),
    ])
  },
}
