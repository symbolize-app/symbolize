import type * as conveyElementAttrTest from '@/elementAttr.test-d.ts'
import type * as conveyMath from '@/math.ts'

export const url = import.meta.url

export const tests = {
  ['math attrs'](): void {
    // All MathML elements share the same class, and no attributes are
    // settable via JS properties, so the test here is very basic
    const tag = 'math'
    let actual: Omit<
      Actual<typeof tag>,
      'altText' | 'display' | 'displayStyle' | 'mathDir' | 'scriptLevel'
    > = null as never
    let expected: Expected<typeof tag> = null as never
    actual = expected
    expected = actual
  },
}

type Actual<Tag extends keyof typeof conveyMath.math> = Readonly<
  Required<Parameters<(typeof conveyMath.math)[Tag]>[0]>
>

type Expected<Tag extends keyof MathMLElementTagNameMap> =
  conveyElementAttrTest.TestAttrs<unknown, MathMLElementTagNameMap[Tag]>
