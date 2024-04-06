import type * as conveyData from '@/data.ts'
import type * as conveyElementAttrs from '@/elementAttrs.ts'
import type * as compute from '@intertwine/lib-compute'

export interface AttrsTagNameMap<CustomContext> {
  readonly math: MathAttrs<CustomContext>
  readonly mi: MiAttrs<CustomContext>
  readonly mo: MoAttrs<CustomContext>
}

export interface Attrs<CustomContext>
  extends conveyElementAttrs.Attrs<CustomContext> {
  readonly dir?: compute.ComputationOpt<'ltr' | 'rtl'>
  readonly displayStyle?: compute.ComputationOpt<boolean>
  readonly scriptLevel?: compute.ComputationOpt<number>
}

export interface MathAttrs<CustomContext> extends Attrs<CustomContext> {
  readonly altText?: compute.ComputationOpt<string>
  readonly display?: compute.ComputationOpt<'block' | 'inline'>
}

export interface MiAttrs<CustomContext> extends Attrs<CustomContext> {
  readonly mathVariant?: compute.ComputationOpt<'normal'>
}

export interface MoAttrs<CustomContext> extends Attrs<CustomContext> {
  readonly fence?: compute.ComputationOpt<boolean>
  readonly form?: compute.ComputationOpt<'infix' | 'postfix' | 'prefix'>
  readonly lSpace?: compute.ComputationOpt<conveyData.Length>
  readonly largeOp?: compute.ComputationOpt<boolean>
  readonly maxSize?: compute.ComputationOpt<conveyData.Length>
  readonly minSize?: compute.ComputationOpt<conveyData.Length>
  readonly moveableLimits?: compute.ComputationOpt<boolean>
  readonly rSpace?: compute.ComputationOpt<conveyData.Length>
  readonly separator?: compute.ComputationOpt<boolean>
  readonly stretchy?: compute.ComputationOpt<boolean>
  readonly symmetric?: compute.ComputationOpt<boolean>
}
