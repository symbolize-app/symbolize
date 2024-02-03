import type * as time from '@intertwine/lib-time'
import * as nodePerfHooks from 'node:perf_hooks'

export function initContext(): time.Context {
  return {
    time: {
      performanceNow() {
        return nodePerfHooks.performance.now()
      },
      setTimeout(callback, ms) {
        return global.setTimeout(callback, ms)
      },
    },
  }
}
