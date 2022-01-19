import type * as route from '@tiny/api/route.ts'

export function initContext(): route.Context {
  return {
    maxRequestNonStreamedBytes: 4 * 1024,
  }
}
