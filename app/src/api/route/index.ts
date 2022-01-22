import type * as tinyRoute from '@tiny/api/route.ts'

export function initContext(): tinyRoute.Context {
  return {
    maxRequestNonStreamedBytes: 4 * 1024,
  }
}
