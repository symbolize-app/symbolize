import type * as conveyElementAttrs from '@/elementAttrs.ts'

export interface MathAttrsTagNameMap<CustomContext> {
  readonly math: MathMathAttrs<CustomContext, MathMLElement>
  readonly mi: MiAttrs<CustomContext, MathMLElement>
  readonly mo: MoAttrs<CustomContext, MathMLElement>
}

export type MathAttrsByElement<
  CustomElement extends MathMLElement,
  CustomContext,
> = {
  [Key in keyof MathAttrsTagNameMap<unknown>]: MathAttrs<
    CustomContext,
    CustomElement
  > extends MathAttrsTagNameMap<CustomContext>[Key] ?
    MathAttrsTagNameMap<CustomContext>[Key]
  : never
}[keyof MathAttrsTagNameMap<unknown>]

export type MathAttrs<
  CustomContext,
  BaseElement extends Element,
> = conveyElementAttrs.Attrs<CustomContext, BaseElement> &
  conveyElementAttrs.PickAttrs<'displayStyle' | 'mathDir' | 'scriptLevel'>

export type MathMathAttrs<
  CustomContext,
  BaseElement extends Element,
> = conveyElementAttrs.PickAttrs<'altText' | 'display'> &
  MathAttrs<CustomContext, BaseElement>

export type MiAttrs<
  CustomContext,
  BaseElement extends Element,
> = conveyElementAttrs.PickAttrs<'mathVariant'> &
  MathAttrs<CustomContext, BaseElement>

export type MoAttrs<
  CustomContext,
  BaseElement extends Element,
> = conveyElementAttrs.PickAttrs<
  | 'fence'
  | 'largeOp'
  | 'lSpace'
  | 'maxSize'
  | 'minSize'
  | 'moveableLimits'
  | 'operatorForm'
  | 'rSpace'
  | 'separator'
  | 'stretchy'
  | 'symmetric'
> &
  MathAttrs<CustomContext, BaseElement>
