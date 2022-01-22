import type * as tinyTime from '@tiny/core/time.ts'
import * as nodePerfHooks from 'node:perf_hooks'

export function initContext(): tinyTime.Context {
  return {
    performanceNow: () => nodePerfHooks.performance.now(),
    setTimeout: (...args) => global.setTimeout(...args),
  }
}
