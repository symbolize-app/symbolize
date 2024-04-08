import type * as conveyElementTest from '@/element.test-d.ts'
import type * as conveyHtml from '@/html.ts'

export const url = import.meta.url

export const tests = {
  ['button attrs'](): void {
    const tag = 'button'
    let actual: Actual<typeof tag> = null as never
    let expected: Expected<typeof tag> = null as never
    actual = expected
    expected = actual
  },

  ['div attrs'](): void {
    const tag = 'div'
    let actual: Actual<typeof tag> = null as never
    let expected: Expected<typeof tag> = null as never
    actual = expected
    expected = actual
  },
}

type Actual<Tag extends keyof typeof conveyHtml.html> = Readonly<
  Required<Parameters<(typeof conveyHtml.html)[Tag]>[0]>
>

type Expected<Tag extends keyof HTMLElementTagNameMap> =
  conveyElementTest.TestAttrs<unknown, HTMLElementTagNameMap[Tag]>
