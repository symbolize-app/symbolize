import type * as time from '@tiny/util/time.ts'
import * as perfHooks from 'perf_hooks'

export function initContext(): time.Context {
  return {
    performanceNow: () => perfHooks.performance.now(),
    setTimeout: (...args) => global.setTimeout(...args),
  }
}
