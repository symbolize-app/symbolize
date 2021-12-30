import type * as time from '@tiny/core/time.ts'
import * as perfHooks from 'node:perf_hooks'

export function initContext(): time.Context {
  return {
    performanceNow: () => perfHooks.performance.now(),
    setTimeout: (...args) => global.setTimeout(...args),
  }
}
