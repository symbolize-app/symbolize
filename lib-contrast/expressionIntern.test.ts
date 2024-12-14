import * as contrast from '@/index.ts'
import * as contrastTest from '@/test.ts'
import * as test from '@symbolize/lib-test'

export const url = import.meta.url

export const tests = {
  async ['multi semicolon complex'](ctx: contrast.Context): Promise<void> {
    const style = [
      contrast.background.color(
        contrast.c(
          contrast.rgb(contrast.pct(0), contrast.pct(0), contrast.pct(0)),
          contrast.c(
            contrast.rgb(
              contrast.pct(0),
              contrast.pct(0),
              contrast.pct(1),
            ),
            contrast.rgb(
              contrast.pct(0),
              contrast.pct(0),
              contrast.pct(2),
            ),
          ),
          contrast.rgb(contrast.pct(0), contrast.pct(0), contrast.pct(3)),
          contrast.select.match(
            contrast.select.hover(),
            contrast.c(
              contrast.rgb(
                contrast.pct(0),
                contrast.pct(0),
                contrast.pct(4),
              ),
              contrast.c(
                contrast.rgb(
                  contrast.pct(0),
                  contrast.pct(0),
                  contrast.pct(5),
                ),
                contrast.rgb(
                  contrast.pct(0),
                  contrast.pct(0),
                  contrast.pct(6),
                ),
              ),
              contrast.rgb(
                contrast.pct(0),
                contrast.pct(0),
                contrast.pct(7),
              ),
            ),
          ),
          contrast.rgb(contrast.pct(0), contrast.pct(0), contrast.pct(8)),
          contrast.rgb(contrast.pct(0), contrast.pct(0), contrast.pct(9)),
        ),
      ),
    ]
    const result = await contrastTest.testCompile(ctx, style)

    test.assertDeepEquals(result.classNames, ['a0'])
    test.assertDeepEquals(result.code, [
      contrastTest.dedent(`
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

  async ['scope expression reuse'](ctx: contrast.Context): Promise<void> {
    const style = [
      contrast.background.color(
        contrast.rgb(
          contrast.pct(0),
          contrast.c(
            contrast.pct(0),
            contrast.select.match(
              contrast.select.hover(),
              contrast.pct(1),
            ),
          ),
          contrast.c(
            contrast.pct(0),
            contrast.select.match(
              contrast.select.hover(),
              contrast.pct(1),
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
          --e0: 0%;
          &:where(:hover) {
            --e0: 1%;
          }
        }
      `),
      contrastTest.dedent(`
        .a0 {
          background-color: rgb(0% var(--e0) var(--e0));
        }
      `),
    ])
  },

  async ['scope expression chaining'](
    ctx: contrast.Context,
  ): Promise<void> {
    const style = [
      contrast.background.color(
        contrast.rgb(
          contrast.pct(0),
          contrast.pct(0),
          contrast.c(
            contrast.pct(0),
            contrast.select.match(
              contrast.select.hover(),
              contrast.add(
                contrast.pct(1),
                contrast.c(
                  contrast.pct(0),
                  contrast.select.match(
                    contrast.select.disabled(),
                    contrast.pct(2),
                  ),
                ),
              ),
            ),
          ),
        ),
      ),
    ]
    const result = await contrastTest.testCompile(ctx, style)

    test.assertDeepEquals(result.classNames, ['e0', 'e1', 'a0'])
    test.assertDeepEquals(result.code, [
      contrastTest.dedent(`
        .e0 {
          --e0: 0%;
          &:where(:disabled) {
            --e0: 2%;
          }
        }
      `),
      contrastTest.dedent(`
        .e1 {
          --e1: 0%;
          &:where(:hover) {
            --e1: calc(1% + var(--e0));
          }
        }
      `),
      contrastTest.dedent(`
        .a0 {
          background-color: rgb(0% 0% var(--e1));
        }
      `),
    ])
  },
}
