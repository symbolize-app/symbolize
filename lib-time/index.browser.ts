import type * as time from '@intertwine/lib-time'

export function initContext(): time.Context {
  return {
    time: {
      performanceNow: () => globalThis.performance.now(),
      setTimeout: (...args) => globalThis.setTimeout(...args),
    },
  }
}
