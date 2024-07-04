import * as contrast from '@/index.ts'

export const url = import.meta.url

export const tests = {
  ['variant / ligatures / empty'](): void {
    // @ts-expect-error -- wrong tuple
    contrast.font.variant.ligatures()
  },

  ['variant / ligatures / normal'](): void {
    contrast.font.variant.ligatures('normal')
  },

  ['variant / ligatures / all'](): void {
    contrast.font.variant.ligatures(
      'common-ligatures',
      'discretionary-ligatures',
      'historical-ligatures',
      'contextual-ligatures',
    )
  },

  ['variant / ligatures / extra'](): void {
    contrast.font.variant.ligatures(
      // @ts-expect-error -- wrong tuple
      'common-ligatures',
      'no-common-ligatures',
      'discretionary-ligatures',
      'historical-ligatures',
      'contextual-ligatures',
    )
  },

  ['variant / ligatures / mixed'](): void {
    contrast.font.variant.ligatures(
      // @ts-expect-error -- wrong tuple
      'normal',
      'common-ligatures',
      'discretionary-ligatures',
      'historical-ligatures',
      'contextual-ligatures',
    )
  },

  ['variant / numeric / empty'](): void {
    // @ts-expect-error -- wrong tuple
    contrast.font.variant.numeric()
  },

  ['variant / numeric / normal'](): void {
    contrast.font.variant.numeric('normal')
  },

  ['variant / numeric / all'](): void {
    contrast.font.variant.numeric(
      'lining-nums',
      'proportional-nums',
      'diagonal-fractions',
      'ordinal',
      'slashed-zero',
    )
  },

  ['variant / numeric / extra'](): void {
    contrast.font.variant.numeric(
      // @ts-expect-error -- wrong tuple
      'lining-nums',
      'oldstyle-nums',
      'proportional-nums',
      'diagonal-fractions',
      'ordinal',
      'slashed-zero',
    )
  },

  ['variant / numeric / mixed'](): void {
    contrast.font.variant.numeric(
      // @ts-expect-error -- wrong tuple
      'normal',
      'lining-nums',
      'proportional-nums',
      'diagonal-fractions',
      'ordinal',
      'slashed-zero',
    )
  },
}
