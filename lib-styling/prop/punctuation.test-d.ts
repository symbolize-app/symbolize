import * as styling from '@/index.ts'

export const url = import.meta.url

export const tests = {
  ['hang / empty'](): void {
    // @ts-expect-error -- wrong tuple
    styling.punctuation.hang()
  },

  ['hang / none'](): void {
    styling.punctuation.hang('none')
  },

  ['hang / all'](): void {
    styling.punctuation.hang('first', 'allow-end', 'last')
  },

  ['hang / extra'](): void {
    styling.punctuation.hang(
      // @ts-expect-error -- wrong tuple
      'first',
      'first',
      'allow-end',
      'last',
    )
  },

  ['hang / mixed'](): void {
    styling.punctuation.hang(
      // @ts-expect-error -- wrong tuple
      'none',
      'first',
      'allow-end',
      'last',
    )
  },
}
