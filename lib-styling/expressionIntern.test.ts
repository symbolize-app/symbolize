import * as styling from '@/index.ts'
import * as stylingTest from '@/test.ts'
import * as test from '@symbolize/lib-test'

export const url = import.meta.url

export const tests = {
  async ['multi semicolon complex'](ctx: styling.Context): Promise<void> {
    const style = [
      styling.background.color(
        styling.c(
          styling.rgb(styling.pct(0), styling.pct(0), styling.pct(0)),
          styling.c(
            styling.rgb(styling.pct(0), styling.pct(0), styling.pct(1)),
            styling.rgb(styling.pct(0), styling.pct(0), styling.pct(2)),
          ),
          styling.rgb(styling.pct(0), styling.pct(0), styling.pct(3)),
          styling.select.match(
            styling.select.hover(),
            styling.c(
              styling.rgb(styling.pct(0), styling.pct(0), styling.pct(4)),
              styling.c(
                styling.rgb(
                  styling.pct(0),
                  styling.pct(0),
                  styling.pct(5),
                ),
                styling.rgb(
                  styling.pct(0),
                  styling.pct(0),
                  styling.pct(6),
                ),
              ),
              styling.rgb(styling.pct(0), styling.pct(0), styling.pct(7)),
            ),
          ),
          styling.rgb(styling.pct(0), styling.pct(0), styling.pct(8)),
          styling.rgb(styling.pct(0), styling.pct(0), styling.pct(9)),
        ),
      ),
    ]
    const result = await stylingTest.testCompile(ctx, style)

    test.assertDeepEquals(result.classNames, ['a0'])
    test.assertDeepEquals(result.code, [
      stylingTest.dedent(`
        .a0 {
          background-color: rgb(0% 0% 0%);
          background-color: rgb(0% 0% 1%);
          background-color: rgb(0% 0% 2%);
          background-color: rgb(0% 0% 3%);
          &:where(:hover) {
            background-color: rgb(0% 0% 4%);
            background-color: rgb(0% 0% 5%);
            background-color: rgb(0% 0% 6%);
            background-color: rgb(0% 0% 7%);
          }
          & {
            background-color: rgb(0% 0% 8%);
            background-color: rgb(0% 0% 9%);
          }
        }
      `),
    ])
  },

  async ['scope expression reuse'](ctx: styling.Context): Promise<void> {
    const style = [
      styling.background.color(
        styling.rgb(
          styling.pct(0),
          styling.c(
            styling.pct(0),
            styling.select.match(styling.select.hover(), styling.pct(1)),
          ),
          styling.c(
            styling.pct(0),
            styling.select.match(styling.select.hover(), styling.pct(1)),
          ),
        ),
      ),
    ]
    const result = await stylingTest.testCompile(ctx, style)

    test.assertDeepEquals(result.classNames, ['e0', 'a0'])
    test.assertDeepEquals(result.code, [
      stylingTest.dedent(`
        .e0 {
          --e0: 0%;
          &:where(:hover) {
            --e0: 1%;
          }
        }
      `),
      stylingTest.dedent(`
        .a0 {
          background-color: rgb(0% var(--e0) var(--e0));
        }
      `),
    ])
  },

  async ['scope expression chaining'](
    ctx: styling.Context,
  ): Promise<void> {
    const style = [
      styling.background.color(
        styling.rgb(
          styling.pct(0),
          styling.pct(0),
          styling.c(
            styling.pct(0),
            styling.select.match(
              styling.select.hover(),
              styling.add(
                styling.pct(1),
                styling.c(
                  styling.pct(0),
                  styling.select.match(
                    styling.select.disabled(),
                    styling.pct(2),
                  ),
                ),
              ),
            ),
          ),
        ),
      ),
    ]
    const result = await stylingTest.testCompile(ctx, style)

    test.assertDeepEquals(result.classNames, ['e0', 'e1', 'a0'])
    test.assertDeepEquals(result.code, [
      stylingTest.dedent(`
        .e0 {
          --e0: 0%;
          &:where(:disabled) {
            --e0: 2%;
          }
        }
      `),
      stylingTest.dedent(`
        .e1 {
          --e1: 0%;
          &:where(:hover) {
            --e1: calc(1% + var(--e0));
          }
        }
      `),
      stylingTest.dedent(`
        .a0 {
          background-color: rgb(0% 0% var(--e1));
        }
      `),
    ])
  },
}
