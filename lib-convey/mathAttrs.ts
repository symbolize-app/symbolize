import type * as conveyElementAttrs from '@/elementAttrs.ts'

export interface AttrsTagNameMap<CustomContext> {
  readonly math: MathAttrs<CustomContext>
  readonly mi: MiAttrs<CustomContext>
  readonly mo: MoAttrs<CustomContext>
}

export type Attrs<CustomContext> =
  conveyElementAttrs.Attrs<CustomContext> &
    conveyElementAttrs.PickAttrs<
      'displayStyle' | 'mathDir' | 'scriptLevel'
    >

export type MathAttrs<CustomContext> = Attrs<CustomContext> &
  conveyElementAttrs.PickAttrs<'altText' | 'display'>

export type MiAttrs<CustomContext> = Attrs<CustomContext> &
  conveyElementAttrs.PickAttrs<'mathVariant'>

export type MoAttrs<CustomContext> = Attrs<CustomContext> &
  conveyElementAttrs.PickAttrs<
    | 'fence'
    | 'form'
    | 'largeOp'
    | 'lSpace'
    | 'maxSize'
    | 'minSize'
    | 'moveableLimits'
    | 'rSpace'
    | 'separator'
    | 'stretchy'
    | 'symmetric'
  >
