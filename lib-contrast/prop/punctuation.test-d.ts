import * as contrast from '@/index.ts'

export const url = import.meta.url

export const tests = {
  ['hang / empty'](): void {
    // @ts-expect-error -- wrong tuple
    contrast.punctuation.hang()
  },

  ['hang / none'](): void {
    contrast.punctuation.hang('none')
  },

  ['hang / all'](): void {
    contrast.punctuation.hang('first', 'allow-end', 'last')
  },

  ['hang / extra'](): void {
    contrast.punctuation.hang(
      // @ts-expect-error -- wrong tuple
      'first',
      'first',
      'allow-end',
      'last',
    )
  },

  ['hang / mixed'](): void {
    contrast.punctuation.hang(
      // @ts-expect-error -- wrong tuple
      'none',
      'first',
      'allow-end',
      'last',
    )
  },
}
