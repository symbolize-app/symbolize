import type * as conveyElementTest from '@/element.test-d.ts'
import type * as conveySvg from '@/svg.ts'

export const url = import.meta.url

export const tests = {
  ['g attrs'](): void {
    const tag = 'g'
    let actual: Actual<typeof tag> = null as never
    let expected: Expected<typeof tag> = null as never
    actual = expected
    expected = actual
  },

  ['rect attrs'](): void {
    const tag = 'rect'
    let actual: Actual<typeof tag> = null as never
    let expected: Expected<typeof tag> = null as never
    actual = expected
    expected = actual
  },

  ['svg attrs'](): void {
    const tag = 'svg'
    let actual: Actual<typeof tag> = null as never
    let expected: Expected<typeof tag> = null as never
    actual = expected
    expected = actual
  },
}

type Actual<Tag extends keyof typeof conveySvg.svg> = Readonly<
  Required<Parameters<(typeof conveySvg.svg)[Tag]>[0]>
>

type Expected<Tag extends keyof SVGElementTagNameMap> =
  conveyElementTest.TestAttrs<unknown, SVGElementTagNameMap[Tag]>
