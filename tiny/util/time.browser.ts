import type * as time from '@tiny/util/time.ts'

export function initContext(): time.Context {
  return {
    performanceNow: () => window.performance.now(),
    setTimeout: (...args) => global.setTimeout(...args),
  }
}
