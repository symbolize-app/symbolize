import type * as time from '@intertwine/lib-time'
import * as nodePerfHooks from 'node:perf_hooks'

export function initContext(): time.Context {
  return {
    time: {
      performanceNow: () => nodePerfHooks.performance.now(),
      setTimeout: (...args) => global.setTimeout(...args),
    },
  }
}
