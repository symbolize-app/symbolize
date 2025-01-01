import type * as hypertextElementAttrTest from '@/elementAttr.test-d.ts'
import type * as hypertextSvg from '@/svg.ts'

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

type Actual<Tag extends keyof typeof hypertextSvg.svg> = Readonly<
  Required<Parameters<(typeof hypertextSvg.svg)[Tag]>[0]>
>

type Expected<Tag extends keyof SVGElementTagNameMap> =
  hypertextElementAttrTest.TestAttrs<unknown, SVGElementTagNameMap[Tag]>
