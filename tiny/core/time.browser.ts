import type * as time from '@tiny/core/time.ts'

export function initContext(): time.Context {
  return {
    performanceNow: () => window.performance.now(),
    setTimeout: (...args) => window.setTimeout(...args),
  }
}
