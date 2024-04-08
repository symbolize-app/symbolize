import type * as conveyElementAttrs from '@/elementAttrs.ts'

export interface AttrsTagNameMap<CustomContext> {
  readonly button: ButtonAttrs<CustomContext>
  readonly div: Attrs<CustomContext>
  readonly span: Attrs<CustomContext>
}

export type Attrs<CustomContext> =
  conveyElementAttrs.Attrs<CustomContext> &
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

export type ButtonAttrs<CustomContext> = Attrs<CustomContext> &
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
