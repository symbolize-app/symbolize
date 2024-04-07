import type * as conveyData from '@/data.ts'
import type * as conveyElementAttrsTest from '@/elementAttrs.test-d.ts'
import type * as conveyFragment from '@/fragment.ts'
import type * as compute from '@intertwine/lib-compute'

export type TestAttrs<
  CustomContext,
  BaseElement extends Element,
> = TestListenerAttrs<BaseElement> &
  TestWritableAttrs<BaseElement> & {
    readonly content: conveyFragment.FragmentOpt<CustomContext>
  }

type TestListenerAttrs<BaseElement extends Element> = {
  readonly [Key in keyof conveyElementAttrsTest.CustomEventMap as conveyElementAttrsTest.CustomEventMap[Key] extends (
    keyof conveyElementAttrsTest.EventMap<BaseElement>
  ) ?
    Key
  : never]: conveyElementAttrsTest.CustomEventMap[Key] extends (
    keyof conveyElementAttrsTest.EventMap<BaseElement>
  ) ?
    (
      event: conveyElementAttrsTest.EventMap<BaseElement>[conveyElementAttrsTest.CustomEventMap[Key]],
    ) => Promise<void> | void
  : never
}

type TestWritableAttrs<BaseElement extends Element> = {
  readonly [Key in Exclude<
    keyof BaseElement,
    | keyof conveyElementAttrsTest.CustomEventMap
    | keyof conveyElementAttrsTest.SkippedMap
    | 'content'
  > as Key extends Uppercase<Key extends string ? Key : never> ? never
  : NonNullable<BaseElement[Key]> extends (
    | HTMLCollection
    | NamedNodeMap
    | Node
    | NodeList
    | ((...args: never) => unknown)
  ) ?
    never
  : Key]: Key extends keyof conveyElementAttrsTest.OverrideMap ?
    conveyElementAttrsTest.OverrideMap[Key]
  : compute.ComputationOpt<
      BaseElement[Key] extends boolean ? boolean
      : BaseElement[Key] extends SVGAnimatedLength ?
        conveyData.SvgLengthOpt | null
      : BaseElement[Key] extends SVGAnimatedNumber ? number | null
      : BaseElement[Key] extends SVGAnimatedPreserveAspectRatio ?
        conveyData.SvgPreserveAspectRatioOpt | null
      : BaseElement[Key] extends SVGAnimatedRect ? conveyData.Rect | null
      : BaseElement[Key] extends SVGStringList ? string[] | null
      : BaseElement[Key] | null
    >
}
