import type * as conveyElementAttrs from '@/elementAttrs.ts'

export interface AttrsTagNameMap<CustomContext> {
  readonly button: ButtonAttrs<CustomContext, HTMLButtonElement>
  readonly div: Attrs<CustomContext, HTMLDivElement>
  readonly span: Attrs<CustomContext, HTMLSpanElement>
}

export type Attrs<
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
> = Attrs<CustomContext, BaseElement> &
  conveyElementAttrs.PickAttrs<
    | 'disabled'
    | 'formAction'
    | 'formEnctype'
    | 'formMethod'
    | 'formNoValidate'
    | 'formTarget'
    | 'name'
    | 'type'
    | 'value'
  >
