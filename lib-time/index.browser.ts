import type * as time from '@intertwine/lib-time'

export function initContext(): time.Context {
  return {
    performanceNow: () => window.performance.now(),
    setTimeout: (...args) => window.setTimeout(...args),
  }
}
