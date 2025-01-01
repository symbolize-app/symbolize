import * as styling from '@/index.ts'
import * as stylingTest from '@/test.ts'
import * as test from '@symbolize/lib-test'

export const url = import.meta.url

export const tests = {
  async ['hover'](ctx: styling.Context): Promise<void> {
    const style = [
      styling.background.color(
        styling.select.match(
          styling.select.hover(),
          styling.rgb(styling.pct(100), styling.pct(0), styling.pct(50)),
        ),
      ),
    ]
    const result = await stylingTest.testCompile(ctx, style)

    test.assertDeepEquals(result.classNames, ['a0'])
    test.assertDeepEquals(result.code, [
      stylingTest.dedent(`
        .a0 {
          &:where(:hover) {
            background-color: rgb(100% 0% 50%);
          }
        }
      `),
    ])
  },

  async ['and'](ctx: styling.Context): Promise<void> {
    const style = [
      styling.background.color(
        styling.select.match(
          styling.select.and(
            styling.select.disabled(),
            styling.select.hover(),
          ),
          styling.rgb(styling.pct(100), styling.pct(0), styling.pct(50)),
        ),
      ),
    ]
    const result = await stylingTest.testCompile(ctx, style)

    test.assertDeepEquals(result.classNames, ['a0'])
    test.assertDeepEquals(result.code, [
      stylingTest.dedent(`
        .a0 {
          &:where(:disabled:hover) {
            background-color: rgb(100% 0% 50%);
          }
        }
      `),
    ])
  },

  async ['not'](ctx: styling.Context): Promise<void> {
    const style = [
      styling.background.color(
        styling.select.match(
          styling.select.not(
            styling.select.disabled(),
            styling.select.hover(),
          ),
          styling.rgb(styling.pct(100), styling.pct(0), styling.pct(50)),
        ),
      ),
    ]
    const result = await stylingTest.testCompile(ctx, style)

    test.assertDeepEquals(result.classNames, ['a0'])
    test.assertDeepEquals(result.code, [
      stylingTest.dedent(`
        .a0 {
          &:where(:not(:disabled:hover)) {
            background-color: rgb(100% 0% 50%);
          }
        }
      `),
    ])
  },

  async ['or'](ctx: styling.Context): Promise<void> {
    const style = [
      styling.background.color(
        styling.select.match(
          styling.select.or(
            styling.select.disabled(),
            styling.select.hover(),
          ),
          styling.rgb(styling.pct(100), styling.pct(0), styling.pct(50)),
        ),
      ),
    ]
    const result = await stylingTest.testCompile(ctx, style)

    test.assertDeepEquals(result.classNames, ['a0'])
    test.assertDeepEquals(result.code, [
      stylingTest.dedent(`
        .a0 {
          &:where(:where(:disabled, :hover)) {
            background-color: rgb(100% 0% 50%);
          }
        }
      `),
    ])
  },

  async ['complex'](ctx: styling.Context): Promise<void> {
    for (let i = 0; i < 2; i++) {
      const style = [
        styling.background.color(
          styling.select.match(
            styling.select.and(
              styling.select.or(
                styling.select.disabled(),
                styling.select.and(
                  styling.select.empty(),
                  styling.select.hover(),
                ),
              ),
              styling.select.not(
                styling.select.firstChild(),
                styling.select.lastChild(),
              ),
            ),
            styling.rgb(styling.pct(100), styling.pct(0), styling.pct(50)),
          ),
        ),
      ]
      const result = await stylingTest.testCompile(ctx, style)

      test.assertDeepEquals(result.classNames, ['a0'])
      test.assertDeepEquals(result.code, [
        stylingTest.dedent(`
        .a0 {
          &:where(:where(:disabled, :empty:hover):not(:first-child:last-child)) {
            background-color: rgb(100% 0% 50%);
          }
        }
      `),
      ])
    }
  },
}
