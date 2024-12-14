import * as convey from '@/index.ts'
import * as contrast from '@symbolize/lib-contrast'
import * as contrastTest from '@symbolize/lib-contrast/test.ts'
import * as test from '@symbolize/lib-test'

export const url = import.meta.url

export const tests = {
  async ['attr / name'](ctx: contrast.Context): Promise<void> {
    const style = [
      contrast.isolation(
        contrast.select.match(
          convey.select.attr({
            accessKey: 'x',
          }),
          'isolate',
        ),
      ),
    ]
    const result = await contrastTest.testCompile(ctx, style)

    test.assertDeepEquals(result.classNames, ['a0'])
    test.assertDeepEquals(result.code, [
      contrastTest.dedent(`
        .a0 {
          &:where([accesskey="x"]) {
            isolation: isolate;
          }
        }
      `),
    ])
  },

  async ['attr / escape'](ctx: contrast.Context): Promise<void> {
    const style = [
      contrast.isolation(
        contrast.select.match(
          convey.select.attr({
            accessKey: '\'"\\,\n',
          }),
          'isolate',
        ),
      ),
    ]
    const result = await contrastTest.testCompile(ctx, style)

    test.assertDeepEquals(result.classNames, ['a0'])
    test.assertDeepEquals(result.code, [
      contrastTest.dedent(`
        .a0 {
          &:where([accesskey="'\\"\\\\,\\n"]) {
            isolation: isolate;
          }
        }
      `),
    ])
  },

  async ['attr / bool true'](ctx: contrast.Context): Promise<void> {
    const style = [
      contrast.isolation(
        contrast.select.match(
          convey.select.attr({
            inert: true,
          }),
          'isolate',
        ),
      ),
    ]
    const result = await contrastTest.testCompile(ctx, style)

    test.assertDeepEquals(result.classNames, ['a0'])
    test.assertDeepEquals(result.code, [
      contrastTest.dedent(`
        .a0 {
          &:where([inert]) {
            isolation: isolate;
          }
        }
      `),
    ])
  },

  async ['attr / bool false'](ctx: contrast.Context): Promise<void> {
    const style = [
      contrast.isolation(
        contrast.select.match(
          convey.select.attr({
            inert: false,
          }),
          'isolate',
        ),
      ),
    ]
    const result = await contrastTest.testCompile(ctx, style)

    test.assertDeepEquals(result.classNames, ['a0'])
    test.assertDeepEquals(result.code, [
      contrastTest.dedent(`
        .a0 {
          &:where(:not([inert])) {
            isolation: isolate;
          }
        }
      `),
    ])
  },

  async ['attr / multi'](ctx: contrast.Context): Promise<void> {
    const style = [
      contrast.isolation(
        contrast.select.match(
          convey.select.attr({
            accessKey: 'x',
            id: 'y',
          }),
          'isolate',
        ),
      ),
    ]
    const result = await contrastTest.testCompile(ctx, style)

    test.assertDeepEquals(result.classNames, ['a0'])
    test.assertDeepEquals(result.code, [
      contrastTest.dedent(`
        .a0 {
          &:where([accesskey="x"][id="y"]) {
            isolation: isolate;
          }
        }
      `),
    ])
  },

  async ['type / multi'](ctx: contrast.Context): Promise<void> {
    const style = [
      contrast.isolation(
        contrast.select.match(
          convey.select.type('button', 'g'),
          'isolate',
        ),
      ),
    ]
    const result = await contrastTest.testCompile(ctx, style)

    test.assertDeepEquals(result.classNames, ['a0'])
    test.assertDeepEquals(result.code, [
      contrastTest.dedent(`
        .a0 {
          &:where(:where(button, g)) {
            isolation: isolate;
          }
        }
      `),
    ])
  },
}
