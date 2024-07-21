import * as contrast from '@/index.ts'
import * as contrastTest from '@/test.ts'
import * as test from '@intertwine/lib-test'

export const url = import.meta.url

export const tests = {
  async ['before basic'](ctx: contrast.Context): Promise<void> {
    const style = [
      contrast.before(
        contrast.background.color(
          contrast.rgb(
            contrast.pct(100),
            contrast.pct(0),
            contrast.pct(50),
          ),
        ),
      ),
    ]
    const result = await contrastTest.testCompile(ctx, style)

    test.assertDeepEquals(result.classNames, ['a0'])
    test.assertDeepEquals(result.code, [
      contrastTest.dedent(`
        .a0::before {
          background-color: rgb(100% 0% 50%);
        }
      `),
    ])
  },

  async ['before advanced'](ctx: contrast.Context): Promise<void> {
    const style = [
      contrast.before(
        contrast.background.color(
          contrast.c(
            contrast.rgb(
              contrast.pct(100),
              contrast.pct(0),
              contrast.pct(49),
            ),
            contrast.select.match(
              contrast.select.hover(),
              contrast.rgb(
                contrast.pct(100),
                contrast.pct(0),
                contrast.pct(50),
              ),
            ),
          ),
        ),
      ),
    ]
    const result = await contrastTest.testCompile(ctx, style)

    test.assertDeepEquals(result.classNames, ['e0', 'a0'])
    test.assertDeepEquals(result.code, [
      contrastTest.dedent(`
        .e0 {
          --e0: rgb(100% 0% 49%);
          &:where(:hover) {
            --e0: rgb(100% 0% 50%);
          }
        }
      `),
      contrastTest.dedent(`
        .a0::before {
          background-color: var(--e0);
        }
      `),
    ])
  },

  ['before error'](): void {
    const error = test.assertThrows(() => [
      contrast.before(
        contrast.after(
          contrast.background.color(
            contrast.c(
              contrast.rgb(
                contrast.pct(100),
                contrast.pct(0),
                contrast.pct(127),
              ),
              contrast.select.match(
                contrast.select.hover(),
                contrast.rgb(
                  contrast.pct(100),
                  contrast.pct(0),
                  contrast.pct(50),
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
