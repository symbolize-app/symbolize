import type * as conveyElementAttrs from '@/elementAttrs.ts'

export interface HtmlAttrsTagNameMap<CustomContext> {
  readonly button: ButtonAttrs<CustomContext, HTMLButtonElement>
  readonly div: HtmlAttrs<CustomContext, HTMLDivElement>
  readonly span: HtmlAttrs<CustomContext, HTMLSpanElement>
  readonly title: HtmlAttrs<CustomContext, HTMLDivElement>
}

export type HtmlAttrsByElement<
  CustomElement extends HTMLElement,
  CustomContext,
> = {
  [Key in keyof HtmlAttrsTagNameMap<unknown>]: HtmlAttrs<
    CustomContext,
    CustomElement
  > extends HtmlAttrsTagNameMap<CustomContext>[Key] ?
    HtmlAttrsTagNameMap<CustomContext>[Key]
  : never
}[keyof HtmlAttrsTagNameMap<unknown>]

export type HtmlAttrs<
  CustomContext,
  BaseElement extends Element,
> = conveyElementAttrs.Attrs<CustomContext, BaseElement> &
  conveyElementAttrs.PickAttrs<
    | 'accessKey'
    | 'autocapitalize'
    | 'contentEditable'
    | 'dir'
    | 'draggable'
    | 'enterKeyHint'
    | 'hidden'
    | 'inert'
    | 'inputMode'
    | 'lang'
    | 'spellcheck'
    | 'title'
    | 'translate'
  >

export type ButtonAttrs<
  CustomContext,
  BaseElement extends Element,
> = conveyElementAttrs.PickAttrs<
  | 'disabled'
  | 'formAction'
  | 'formEnctype'
  | 'formMethod'
  | 'formNoValidate'
  | 'formTarget'
  | 'name'
  | 'type'
  | 'value'
> &
  HtmlAttrs<CustomContext, BaseElement>
