import * as styling from '@/index.ts'
import * as stylingTest from '@/test.ts'
import * as test from '@symbolize/lib-test'

export const url = import.meta.url

export const tests = {
  async ['before basic'](ctx: styling.Context): Promise<void> {
    const style = [
      styling.before(
        styling.background.color(
          styling.rgb(styling.pct(100), styling.pct(0), styling.pct(50)),
        ),
      ),
    ]
    const result = await stylingTest.testCompile(ctx, style)

    test.assertDeepEquals(result.classNames, ['a0'])
    test.assertDeepEquals(result.code, [
      stylingTest.dedent(`
        .a0::before {
          background-color: rgb(100% 0% 50%);
        }
      `),
    ])
  },

  async ['before advanced'](ctx: styling.Context): Promise<void> {
    const style = [
      styling.before(
        styling.background.color(
          styling.c(
            styling.rgb(styling.pct(100), styling.pct(0), styling.pct(49)),
            styling.select.match(
              styling.select.hover(),
              styling.rgb(
                styling.pct(100),
                styling.pct(0),
                styling.pct(50),
              ),
            ),
          ),
        ),
      ),
    ]
    const result = await stylingTest.testCompile(ctx, style)

    test.assertDeepEquals(result.classNames, ['e0', 'a0'])
    test.assertDeepEquals(result.code, [
      stylingTest.dedent(`
        .e0 {
          --e0: rgb(100% 0% 49%);
          &:where(:hover) {
            --e0: rgb(100% 0% 50%);
          }
        }
      `),
      stylingTest.dedent(`
        .a0::before {
          background-color: var(--e0);
        }
      `),
    ])
  },

  ['before error'](): void {
    const error = test.assertThrows(() => [
      styling.before(
        styling.after(
          styling.background.color(
            styling.c(
              styling.rgb(
                styling.pct(100),
                styling.pct(0),
                styling.pct(127),
              ),
              styling.select.match(
                styling.select.hover(),
                styling.rgb(
                  styling.pct(100),
                  styling.pct(0),
                  styling.pct(50),
                ),
              ),
            ),
          ),
        ),
      ),
    ])
    test.assertInstanceOf(error, Error)
    test.assertEquals(
      error.message,
      'background-color atom already has ::after pseudo-element',
    )
  },
}
