import type * as conveyElementAttrs from '@/elementAttrs.ts'
import type * as compute from '@intertwine/lib-compute'

export interface AttrsTagNameMap<CustomContext> {
  readonly button: ButtonAttrs<CustomContext>
  readonly div: Attrs<CustomContext>
  readonly span: Attrs<CustomContext>
}

export interface Attrs<CustomContext>
  extends conveyElementAttrs.Attrs<CustomContext> {
  readonly accessKey?: compute.ComputationOpt<string>
  readonly autocapitalize?: compute.ComputationOpt<
    'characters' | 'none' | 'sentences' | 'words'
  >
  readonly contentEditable?: compute.ComputationOpt<string>
  readonly dir?: compute.ComputationOpt<'auto' | 'ltr' | 'rtl'>
  readonly draggable?: compute.ComputationOpt<boolean>
  readonly enterKeyHint?: compute.ComputationOpt<
    'done' | 'enter' | 'go' | 'next' | 'previous' | 'search' | 'send'
  >
  readonly hidden?: compute.ComputationOpt<boolean | 'until-found'>
  readonly inert?: compute.ComputationOpt<boolean>
  readonly inputMode?: compute.ComputationOpt<
    | 'decimal'
    | 'email'
    | 'none'
    | 'numeric'
    | 'search'
    | 'tel'
    | 'text'
    | 'url'
  >
  readonly lang?: compute.ComputationOpt<string>
  readonly spellcheck?: compute.ComputationOpt<boolean>
  readonly title?: compute.ComputationOpt<string>
  readonly translate?: compute.ComputationOpt<boolean>
}

export interface ButtonAttrs<CustomContext> extends Attrs<CustomContext> {
  readonly disabled?: compute.ComputationOpt<boolean>
  readonly formAction?: compute.ComputationOpt<string>
  readonly formEnctype?: compute.ComputationOpt<
    | 'application/x-www-form-urlencoded'
    | 'multipart/form-data'
    | 'text/plain'
  >
  readonly formMethod?: compute.ComputationOpt<'dialog' | 'get' | 'post'>
  readonly formNoValidate?: compute.ComputationOpt<boolean>
  readonly formTarget?: compute.ComputationOpt<string>
  readonly name?: compute.ComputationOpt<string>
  readonly type?: compute.ComputationOpt<'button' | 'reset' | 'submit'>
  readonly value?: compute.ComputationOpt<string>
}
