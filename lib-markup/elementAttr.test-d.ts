import type * as markupData from '@/data.ts'
import type * as markupElementAttrTest from '@/elementAttr.test-d.ts'
import type * as markupElementAttr from '@/elementAttr.ts'
import type * as dataflow from '@symbolize/lib-dataflow'

export type TestAttrs<
  CustomContext,
  BaseElement extends Element,
> = Required<
  Pick<
    markupElementAttr.Attrs<CustomContext, BaseElement>,
    'content' | 'onAdd' | 'style'
  >
> &
  TestListenerAttrs<BaseElement> &
  TestWritableAttrs<BaseElement>

type TestListenerAttrs<BaseElement extends Element> = {
  readonly [Key in keyof markupElementAttr.AllAttrs as Key extends (
    `on${infer Type}`
  ) ?
    Lowercase<Type> extends (
      keyof markupElementAttrTest.EventMap<BaseElement>
    ) ?
      Key
    : never
  : never]: Key extends `on${infer Type}` ?
    Lowercase<Type> extends infer LowercaseType ?
      LowercaseType extends (
        keyof markupElementAttrTest.EventMap<BaseElement>
      ) ?
        (
          event: markupElementAttrTest.EventMap<BaseElement>[LowercaseType],
        ) => Promise<void> | void
      : never
    : never
  : never
}

type TestWritableAttrs<BaseElement extends Element> = {
  readonly [Key in Exclude<
    keyof BaseElement,
    keyof markupElementAttrTest.SkippedMap | 'content' | 'onAdd'
  > as Key extends keyof markupElementAttrTest.OverrideMap ? Key
  : Key extends Uppercase<Key extends string ? Key : never> ? never
  : NonNullable<BaseElement[Key]> extends (
    | HTMLCollection
    | NamedNodeMap
    | Node
    | NodeList
    | ((...args: never) => unknown)
  ) ?
    never
  : Key]: Key extends keyof markupElementAttrTest.OverrideMap ?
    markupElementAttrTest.OverrideMap[Key]
  : dataflow.NodeOpt<
      BaseElement[Key] extends boolean ? boolean
      : BaseElement[Key] extends SVGAnimatedLength ?
        markupData.SvgLengthPctOpt | null
      : BaseElement[Key] extends SVGAnimatedNumber ? number | null
      : BaseElement[Key] extends SVGAnimatedPreserveAspectRatio ?
        markupData.SvgPreserveAspectRatioOpt | null
      : BaseElement[Key] extends SVGAnimatedRect ? markupData.Rect | null
      : BaseElement[Key] extends SVGStringList ? readonly string[] | null
      : BaseElement[Key] | null
    >
}

export type EventMap<BaseElement extends Element> = Readonly<
  BaseElement extends HTMLVideoElement ? HTMLVideoElementEventMap
  : BaseElement extends HTMLMediaElement ? HTMLMediaElementEventMap
  : BaseElement extends HTMLBodyElement ? HTMLBodyElementEventMap
  : BaseElement extends HTMLElement ? HTMLElementEventMap
  : BaseElement extends SVGSVGElement ? SVGSVGElementEventMap
  : BaseElement extends SVGElement ? SVGElementEventMap
  : BaseElement extends MathMLElement ? MathMLElementEventMap
  : never
>

export interface SkippedMap {
  readonly accessKeyLabel: never
  readonly align: never
  readonly attributeStyleMap: never
  readonly ['baseURI']: never
  readonly childElementCount: never
  readonly classList: never
  readonly clientHeight: never
  readonly clientLeft: never
  readonly clientTop: never
  readonly clientWidth: never
  readonly currentScale: never
  readonly currentTranslate: never
  readonly dataset: never
  readonly defaultChecked: never
  readonly defaultValue: never
  readonly ['innerHTML']: never
  readonly innerText: never
  readonly isConnected: never
  readonly isContentEditable: never
  readonly localName: never
  readonly ['namespaceURI']: never
  readonly nodeName: never
  readonly nodeType: never
  readonly nodeValue: never
  readonly offsetHeight: never
  readonly offsetLeft: never
  readonly offsetTop: never
  readonly offsetWidth: never
  readonly ['outerHTML']: never
  readonly outerText: never
  readonly part: never
  readonly popover: never
  readonly popoverTargetAction: never
  readonly prefix: never
  readonly scrollHeight: never
  readonly scrollLeft: never
  readonly scrollTop: never
  readonly scrollWidth: never
  readonly selectionDirection: never
  readonly selectionEnd: never
  readonly selectionStart: never
  readonly style: never
  readonly tagName: never
  readonly textContent: never
  readonly transform: never
  readonly type: never
  readonly validationMessage: never
  readonly validity: never
  readonly valueAsDate: never
  readonly valueAsNumber: never
  readonly webkitEntries: never
  readonly webkitdirectory: never
  readonly willValidate: never
}

export type OverrideMap = Required<
  markupElementAttr.PickAttrs<
    | 'ariaAtomic'
    | 'ariaAutoComplete'
    | 'ariaBusy'
    | 'ariaChecked'
    | 'ariaCurrent'
    | 'ariaDisabled'
    | 'ariaExpanded'
    | 'ariaHasPopup'
    | 'ariaHidden'
    | 'ariaLive'
    | 'ariaModal'
    | 'ariaMultiLine'
    | 'ariaMultiSelectable'
    | 'ariaOrientation'
    | 'ariaPressed'
    | 'ariaReadOnly'
    | 'ariaRequired'
    | 'ariaSelected'
    | 'ariaSort'
    | 'autocapitalize'
    | 'autocomplete'
    | 'className'
    | 'contentEditable'
    | 'dir'
    | 'draggable'
    | 'enterKeyHint'
    | 'form'
    | 'formEnctype'
    | 'formMethod'
    | 'hidden'
    | 'inputMode'
    | 'list'
    | 'nonce'
    | 'spellcheck'
    | 'translate'
  >
>
