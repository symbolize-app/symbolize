import * as markup from '@/index.ts'
import * as styling from '@symbolize/lib-styling'
import * as stylingTest from '@symbolize/lib-styling/test.ts'
import * as test from '@symbolize/lib-test'

export const url = import.meta.url

export const tests = {
  async ['attr / name'](ctx: styling.Context): Promise<void> {
    const style = [
      styling.isolation(
        styling.select.match(
          markup.select.attr({
            accessKey: 'x',
          }),
          'isolate',
        ),
      ),
    ]
    const result = await stylingTest.testCompile(ctx, style)

    test.assertDeepEquals(result.classNames, ['a0'])
    test.assertDeepEquals(result.code, [
      stylingTest.dedent(`
        .a0 {
          &:where([accesskey="x"]) {
            isolation: isolate;
          }
        }
      `),
    ])
  },

  async ['attr / escape'](ctx: styling.Context): Promise<void> {
    const style = [
      styling.isolation(
        styling.select.match(
          markup.select.attr({
            accessKey: '\'"\\,\n',
          }),
          'isolate',
        ),
      ),
    ]
    const result = await stylingTest.testCompile(ctx, style)

    test.assertDeepEquals(result.classNames, ['a0'])
    test.assertDeepEquals(result.code, [
      stylingTest.dedent(`
        .a0 {
          &:where([accesskey="'\\"\\\\,\\n"]) {
            isolation: isolate;
          }
        }
      `),
    ])
  },

  async ['attr / bool true'](ctx: styling.Context): Promise<void> {
    const style = [
      styling.isolation(
        styling.select.match(
          markup.select.attr({
            inert: true,
          }),
          'isolate',
        ),
      ),
    ]
    const result = await stylingTest.testCompile(ctx, style)

    test.assertDeepEquals(result.classNames, ['a0'])
    test.assertDeepEquals(result.code, [
      stylingTest.dedent(`
        .a0 {
          &:where([inert]) {
            isolation: isolate;
          }
        }
      `),
    ])
  },

  async ['attr / bool false'](ctx: styling.Context): Promise<void> {
    const style = [
      styling.isolation(
        styling.select.match(
          markup.select.attr({
            inert: false,
          }),
          'isolate',
        ),
      ),
    ]
    const result = await stylingTest.testCompile(ctx, style)

    test.assertDeepEquals(result.classNames, ['a0'])
    test.assertDeepEquals(result.code, [
      stylingTest.dedent(`
        .a0 {
          &:where(:not([inert])) {
            isolation: isolate;
          }
        }
      `),
    ])
  },

  async ['attr / multi'](ctx: styling.Context): Promise<void> {
    const style = [
      styling.isolation(
        styling.select.match(
          markup.select.attr({
            accessKey: 'x',
            id: 'y',
          }),
          'isolate',
        ),
      ),
    ]
    const result = await stylingTest.testCompile(ctx, style)

    test.assertDeepEquals(result.classNames, ['a0'])
    test.assertDeepEquals(result.code, [
      stylingTest.dedent(`
        .a0 {
          &:where([accesskey="x"][id="y"]) {
            isolation: isolate;
          }
        }
      `),
    ])
  },

  async ['type / multi'](ctx: styling.Context): Promise<void> {
    const style = [
      styling.isolation(
        styling.select.match(markup.select.type('button', 'g'), 'isolate'),
      ),
    ]
    const result = await stylingTest.testCompile(ctx, style)

    test.assertDeepEquals(result.classNames, ['a0'])
    test.assertDeepEquals(result.code, [
      stylingTest.dedent(`
        .a0 {
          &:where(:where(button, g)) {
            isolation: isolate;
          }
        }
      `),
    ])
  },
}
