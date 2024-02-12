import type * as time from '@intertwine/lib-time'
import * as nodePerfHooks from 'node:perf_hooks'

export class TimeImpl implements time.Time {
  performanceNow(): number {
    return nodePerfHooks.performance.now()
  }

  setTimeout(callback: () => void, ms: number): unknown {
    return global.setTimeout(callback, ms)
  }
}
