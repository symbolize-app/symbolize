import type * as tinyTime from '@tiny/core/time.ts'

export function initContext(): tinyTime.Context {
  return {
    performanceNow: () => window.performance.now(),
    setTimeout: (...args) => window.setTimeout(...args),
  }
}
