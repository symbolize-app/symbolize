import type * as conveyElementAttrs from '@/elementAttrs.ts'

export interface HtmlAttrsTagNameMap<CustomContext> {
  readonly button: ButtonAttrs<CustomContext, HTMLButtonElement>
  readonly div: HtmlAttrs<CustomContext, HTMLDivElement>
  readonly h1: HtmlAttrs<CustomContext, HTMLHeadingElement>
  readonly h2: HtmlAttrs<CustomContext, HTMLHeadingElement>
  readonly h3: HtmlAttrs<CustomContext, HTMLHeadingElement>
  readonly h4: HtmlAttrs<CustomContext, HTMLHeadingElement>
  readonly h5: HtmlAttrs<CustomContext, HTMLHeadingElement>
  readonly h6: HtmlAttrs<CustomContext, HTMLHeadingElement>
  readonly hr: HtmlAttrs<CustomContext, HTMLHRElement>
  readonly input: InputAttrs<CustomContext, HTMLInputElement>
  readonly p: HtmlAttrs<CustomContext, HTMLParagraphElement>
  readonly span: HtmlAttrs<CustomContext, HTMLSpanElement>
  readonly title: HtmlAttrs<CustomContext, HTMLDivElement>
}

export type HtmlAttrsByElement<
  CustomElement extends HTMLElement,
  CustomContext,
> = {
  [Key in keyof HtmlAttrsTagNameMap<CustomContext> &
    keyof HTMLElementTagNameMap]: HTMLElementTagNameMap[Key] extends (
    CustomElement
  ) ?
    HtmlAttrsTagNameMap<CustomContext>[Key]
  : never
}[keyof HtmlAttrsTagNameMap<CustomContext> & keyof HTMLElementTagNameMap]

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
> = HtmlAttrs<CustomContext, BaseElement> &
  (
    | conveyElementAttrs.PickAttrsForType<
        'submit',
        | 'disabled'
        | 'form'
        | 'formAction'
        | 'formEnctype'
        | 'formMethod'
        | 'formNoValidate'
        | 'formTarget'
        | 'name'
        | 'value'
      >
    | conveyElementAttrs.PickAttrsForType<'button', 'disabled'>
    | conveyElementAttrs.PickAttrsForType<'reset', 'disabled' | 'form'>
  )

export type InputAttrs<
  CustomContext,
  BaseElement extends Element,
> = HtmlAttrs<CustomContext, BaseElement> &
  (
    | conveyElementAttrs.PickAttrsForType<
        'checkbox',
        'checked' | 'disabled' | 'form' | 'name' | 'required' | 'value'
      >
    | conveyElementAttrs.PickAttrsForType<
        'text',
        | 'autocomplete'
        | 'disabled'
        | 'form'
        | 'list'
        | 'maxLength'
        | 'minLength'
        | 'name'
        | 'pattern'
        | 'placeholder'
        | 'readOnly'
        | 'required'
        | 'size'
        | 'value'
      >
  )
