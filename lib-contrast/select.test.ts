import * as contrast from '@/index.ts'
import * as contrastTest from '@/test.ts'
import * as test from '@symbolize/lib-test'

export const url = import.meta.url

export const tests = {
  async ['hover'](ctx: contrast.Context): Promise<void> {
    const style = [
      contrast.background.color(
        contrast.select.match(
          contrast.select.hover(),
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
        .a0 {
          &:where(:hover) {
            background-color: rgb(100% 0% 50%);
          }
        }
      `),
    ])
  },

  async ['and'](ctx: contrast.Context): Promise<void> {
    const style = [
      contrast.background.color(
        contrast.select.match(
          contrast.select.and(
            contrast.select.disabled(),
            contrast.select.hover(),
          ),
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
        .a0 {
          &:where(:disabled:hover) {
            background-color: rgb(100% 0% 50%);
          }
        }
      `),
    ])
  },

  async ['not'](ctx: contrast.Context): Promise<void> {
    const style = [
      contrast.background.color(
        contrast.select.match(
          contrast.select.not(
            contrast.select.disabled(),
            contrast.select.hover(),
          ),
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
        .a0 {
          &:where(:not(:disabled:hover)) {
            background-color: rgb(100% 0% 50%);
          }
        }
      `),
    ])
  },

  async ['or'](ctx: contrast.Context): Promise<void> {
    const style = [
      contrast.background.color(
        contrast.select.match(
          contrast.select.or(
            contrast.select.disabled(),
            contrast.select.hover(),
          ),
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
        .a0 {
          &:where(:where(:disabled, :hover)) {
            background-color: rgb(100% 0% 50%);
          }
        }
      `),
    ])
  },

  async ['complex'](ctx: contrast.Context): Promise<void> {
    for (let i = 0; i < 2; i++) {
      const style = [
        contrast.background.color(
          contrast.select.match(
            contrast.select.and(
              contrast.select.or(
                contrast.select.disabled(),
                contrast.select.and(
                  contrast.select.empty(),
                  contrast.select.hover(),
                ),
              ),
              contrast.select.not(
                contrast.select.firstChild(),
                contrast.select.lastChild(),
              ),
            ),
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
