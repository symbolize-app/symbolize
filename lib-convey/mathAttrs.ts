import type * as conveyElementAttrs from '@/elementAttrs.ts'

export interface AttrsTagNameMap<CustomContext> {
  readonly math: MathAttrs<CustomContext, MathMLElement>
  readonly mi: MiAttrs<CustomContext, MathMLElement>
  readonly mo: MoAttrs<CustomContext, MathMLElement>
}

export type Attrs<
  CustomContext,
  BaseElement extends Element,
> = conveyElementAttrs.Attrs<CustomContext, BaseElement> &
  conveyElementAttrs.PickAttrs<'displayStyle' | 'mathDir' | 'scriptLevel'>

export type MathAttrs<CustomContext, BaseElement extends Element> = Attrs<
  CustomContext,
  BaseElement
> &
  conveyElementAttrs.PickAttrs<'altText' | 'display'>

export type MiAttrs<CustomContext, BaseElement extends Element> = Attrs<
  CustomContext,
  BaseElement
> &
  conveyElementAttrs.PickAttrs<'mathVariant'>

export type MoAttrs<CustomContext, BaseElement extends Element> = Attrs<
  CustomContext,
  BaseElement
> &
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
