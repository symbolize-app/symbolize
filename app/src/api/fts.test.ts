import type * as appFts from '@fe/api/fts.ts'

export function initContext(): appFts.Context {
  return {
    fts: {
      origin: 'https://fts/',
      password: 'FTS',
    },
  }
}
