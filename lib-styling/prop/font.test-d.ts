import * as styling from '@/index.ts'

export const url = import.meta.url

export const tests = {
  ['variant / ligatures / empty'](): void {
    // @ts-expect-error -- wrong tuple
    styling.font.variant.ligatures()
  },

  ['variant / ligatures / normal'](): void {
    styling.font.variant.ligatures('normal')
  },

  ['variant / ligatures / all'](): void {
    styling.font.variant.ligatures(
      'common-ligatures',
      'discretionary-ligatures',
      'historical-ligatures',
      'contextual-ligatures',
    )
  },

  ['variant / ligatures / extra'](): void {
    styling.font.variant.ligatures(
      // @ts-expect-error -- wrong tuple
      'common-ligatures',
      'no-common-ligatures',
      'discretionary-ligatures',
      'historical-ligatures',
      'contextual-ligatures',
    )
  },

  ['variant / ligatures / mixed'](): void {
    styling.font.variant.ligatures(
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
    styling.font.variant.numeric()
  },

  ['variant / numeric / normal'](): void {
    styling.font.variant.numeric('normal')
  },

  ['variant / numeric / all'](): void {
    styling.font.variant.numeric(
      'lining-nums',
      'proportional-nums',
      'diagonal-fractions',
      'ordinal',
      'slashed-zero',
    )
  },

  ['variant / numeric / extra'](): void {
    styling.font.variant.numeric(
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
    styling.font.variant.numeric(
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
